{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Ruleta
  ( BetOption(..)
  , Model(..)
  , Action(..)
  , initialModel
  , updateModel
  , viewModel
  ) where

import Miso
import Miso.Html
import Miso.Html.Property
import Miso.String (MisoString, ms)
import qualified Miso.CSS as Style
import Control.Concurrent (threadDelay)
import GHC.Generics
import Data.Maybe (isNothing)
import Control.Monad.State.Class (put, get)
import System.CPUTime (getCPUTime)
import Unsafe.Coerce (unsafeCoerce)
#ifdef WASM
import GHC.Wasm.Prim (JSVal)
#endif

-- | Get a random integer between low and high (inclusive) using pure Haskell and getCPUTime
getRandomInt :: Int -> Int -> IO Int
getRandomInt low high = do
  t <- getCPUTime
  let seed = fromIntegral (t `mod` 2147483647) :: Integer
      r = (1103515245 * seed + 12345) `mod` 2147483648
  return $ low + fromIntegral (r `mod` fromIntegral (high - low + 1))

-- | Local audio path for Spanish announcement
speechPath :: Int -> String
speechPath num = "voice/number_" ++ show num ++ ".mp3"

#ifdef WASM
foreign import javascript unsafe "let a = document.getElementById($1); if (a) { a.volume = $2; a.currentTime = 0; a.play().catch(e => {}); }"
  js_playSFX :: JSVal -> Double -> IO ()

foreign import javascript unsafe "let a = document.getElementById($2); if (a) { a.volume = 0.30; if ($1) { a.play().catch(e => {}); } else { a.pause(); } }"
  js_toggleMusic :: Bool -> JSVal -> IO ()

foreign import javascript unsafe "if (typeof confetti === 'function') { confetti({ particleCount: 150, spread: 80, origin: { y: 0.6 }, zIndex: 99999 }); }"
  js_triggerConfetti :: IO ()

foreign import javascript unsafe "let v = new Audio($1); if (v) { if (window.currentVoice) { window.currentVoice.pause(); } window.currentVoice = v; v.volume = 1.0; let ctx = window.audioCtx; let gainNode = window.gainNode; try { if (!ctx) { ctx = new (window.AudioContext || window.webkitAudioContext)(); gainNode = ctx.createGain(); gainNode.gain.value = 4.5; gainNode.connect(ctx.destination); window.audioCtx = ctx; window.gainNode = gainNode; } if (ctx.state === 'suspended') { ctx.resume(); } let src = ctx.createMediaElementSource(v); src.connect(gainNode); } catch (e) {} v.play().catch(e => {}); }"
  js_playVoice :: JSVal -> IO ()
#else
type JSVal = String

js_playSFX :: JSVal -> Double -> IO ()
js_playSFX _ _ = pure ()

js_toggleMusic :: Bool -> JSVal -> IO ()
js_toggleMusic _ _ = pure ()

js_triggerConfetti :: IO ()
js_triggerConfetti = pure ()

js_playVoice :: JSVal -> IO ()
js_playVoice _ = pure ()
#endif

-- | Betting options
data BetOption
  = BetRed
  | BetBlack
  | BetGreen
  | BetNumber Int         -- Bet on single number 0-36
  | BetDoz1               -- 1st 12 (1-12)
  | BetDoz2               -- 2nd 12 (13-24)
  | BetDoz3               -- 3rd 12 (25-36)
  | BetLow                -- Low half (1-18)
  | BetHigh               -- High half (19-36)
  | BetEven               -- Even numbers
  | BetOdd                -- Odd numbers
  deriving (Show, Eq, Generic)

-- | Application Model
data Model = Model
  { balance             :: Int
  , currentBetAmount    :: Int
  , activeBets          :: [(BetOption, [Int])] -- Placed bets with lists of chip values
  , lastRoll            :: Maybe (Int, String)  -- (number, color)
  , history             :: [(Int, String)]      -- list of previous rolls
  , spinning            :: Bool
  , message             :: String
  , winningTarget       :: Maybe Int            -- Target number for the current spin
  , musicPlaying        :: Bool
  , showWinPopup        :: Maybe Int            -- Just winAmount shows popup, Nothing hides it
  , hoveredOption       :: Maybe BetOption      -- Current hovered bet option for highlights
  , showStartModal      :: Bool                 -- True shows the initial start popup
  } deriving (Show, Eq, Generic)

-- | Application Actions
data Action
  = Spin
  | StartSpin Int
  | HandleResult Int
  | SelectChip Int
  | PlaceBet BetOption
  | RemoveBet BetOption
  | HoverBet (Maybe BetOption)
  | ClearBets
  | ResetGame
  | ToggleMusic
  | CloseWinPopup
  | StartGame
  | NoOp
  deriving (Show, Eq)

initialModel :: Model
initialModel = Model
  { balance             = 1000
  , currentBetAmount    = 10
  , activeBets          = []
  , lastRoll            = Nothing
  , history             = []
  , spinning            = False
  , message             = "Bienvenido. Selecciona tu chip, haz tu apuesta y gira la ruleta."
  , winningTarget       = Nothing
  , musicPlaying        = False
  , showWinPopup        = Nothing
  , hoveredOption       = Nothing
  , showStartModal      = True
  }

-- | European roulette numbers in clockwise order on the wheel
wheelNumbers :: [Int]
wheelNumbers = [0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26]

-- | Helper to find the index of an element in a list
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = go 0 xs
  where
    go _ [] = 0
    go i (y:ys) | x == y    = i
                | otherwise = go (i+1) ys

-- | Calculate the angle for a given roulette number based on its position on the wheel
numberAngle :: Int -> Double
numberAngle num = fromIntegral (indexOf num wheelNumbers) * (360.0 / 37.0)

-- | Helper to determine the color of a roulette number
numberColor :: Int -> String
numberColor 0 = "green"
numberColor n
  | n `elem` [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36] = "red"
  | otherwise = "black"

-- | Translate color name to Spanish
translateColor :: String -> String
translateColor "red" = "rojo"
translateColor "black" = "negro"
translateColor "green" = "verde"
translateColor c = c

-- | Calculate payout for a given bet and rolled number
calculatePayout :: BetOption -> Int -> Int -> Int
calculatePayout bet betAmt num =
  case bet of
    BetNumber n  -> if num == n then betAmt * 36 else 0
    BetGreen     -> if num == 0 then betAmt * 36 else 0
    BetRed       -> if numberColor num == "red" then betAmt * 2 else 0
    BetBlack     -> if numberColor num == "black" then betAmt * 2 else 0
    BetDoz1      -> if num >= 1 && num <= 12 then betAmt * 3 else 0
    BetDoz2      -> if num >= 13 && num <= 24 then betAmt * 3 else 0
    BetDoz3      -> if num >= 25 && num <= 36 then betAmt * 3 else 0
    BetLow       -> if num >= 1 && num <= 18 then betAmt * 2 else 0
    BetHigh      -> if num >= 19 && num <= 36 then betAmt * 2 else 0
    BetEven      -> if num > 0 && num `mod` 2 == 0 then betAmt * 2 else 0
    BetOdd       -> if num `mod` 2 /= 0 then betAmt * 2 else 0

-- | Helper to sum all chip values on the board
totalBetAmount :: Model -> Int
totalBetAmount m = sum [ sum chips | (_, chips) <- activeBets m ]

-- | Helper to update the chip list for a given bet option
updateBets :: BetOption -> Int -> [(BetOption, [Int])] -> Maybe [(BetOption, [Int])]
updateBets opt val bets =
  case lookup opt bets of
    Just chips ->
      if length chips >= 5
        then Nothing -- limit of 5 chips exceeded
        else Just $ (opt, chips ++ [val]) : filter (\(o, _) -> o /= opt) bets
    Nothing ->
      Just $ (opt, [val]) : bets

-- | Helper to prevent browser context menu and trigger right click actions
onContextMenu :: action -> Attribute action
onContextMenu act = onWithOptions BUBBLE defaultOptions { _preventDefault = True } "contextmenu" emptyDecoder (\() _ -> act)

-- | Helper to check if a specific number should be highlighted based on the hovered option
shouldHighlightNumber :: Int -> Maybe BetOption -> Bool
shouldHighlightNumber n = \case
  Just (BetNumber num) -> n == num
  Just BetGreen        -> n == 0
  Just BetRed          -> numberColor n == "red"
  Just BetBlack        -> numberColor n == "black"
  Just BetEven         -> n > 0 && n `mod` 2 == 0
  Just BetOdd          -> n `mod` 2 /= 0
  Just BetLow          -> n >= 1 && n <= 18
  Just BetHigh         -> n >= 19 && n <= 36
  Just BetDoz1         -> n >= 1 && n <= 12
  Just BetDoz2         -> n >= 13 && n <= 24
  Just BetDoz3         -> n >= 25 && n <= 36
  Nothing              -> False

-- | Helper to check if a regular cell option should be highlighted
isCellHighlighted :: BetOption -> Model -> Bool
isCellHighlighted opt m =
  case hoveredOption m of
    Just hovered -> hovered == opt
    Nothing      -> False

-- | Update the model based on actions
updateModel :: Action -> Effect ROOT Model Action
updateModel action = case action of
  NoOp -> pure ()

  CloseWinPopup -> do
    m <- get
    put m { showWinPopup = Nothing }

  StartGame -> do
    m <- get
    let newModel = m { showStartModal = False, musicPlaying = True }
    newModel <# do
      js_toggleMusic True (unsafeCoerce ("sound-music" :: MisoString))
      js_playSFX (unsafeCoerce ("sound-click" :: MisoString)) 0.8
      return NoOp

  ResetGame -> do
    m <- get
    let newModel = initialModel
                     { musicPlaying = musicPlaying m
                     , showStartModal = False
                     }
    newModel <# do
      js_playSFX (unsafeCoerce ("sound-click" :: MisoString)) 0.8
      return NoOp

  ToggleMusic -> do
    m <- get
    let nextState = not (musicPlaying m)
        newModel = m { musicPlaying = nextState }
    newModel <# do
      js_toggleMusic nextState (unsafeCoerce ("sound-music" :: MisoString))
      return NoOp

  SelectChip amt -> do
    m <- get
    if spinning m || balance m < amt
      then pure ()
      else
        let newModel = m { currentBetAmount = amt }
        in newModel <# do
             js_playSFX (unsafeCoerce ("sound-click" :: MisoString)) 0.8
             return NoOp

  PlaceBet option -> do
    m <- get
    if spinning m || balance m < currentBetAmount m
      then pure ()
      else
        let selectedVal = currentBetAmount m
            existingBets = activeBets m
            newBets = updateBets option selectedVal existingBets
        in case newBets of
             Just updatedBets ->
               let newBalance = balance m - selectedVal
                   nextBetAmt = bestAffordableChip newBalance (currentBetAmount m)
                   newModel = m { activeBets = updatedBets
                                , balance = newBalance
                                , currentBetAmount = nextBetAmt
                                , message = "Ficha colocada. Hagan juego."
                                }
               in newModel <# do
                    js_playSFX (unsafeCoerce ("sound-click" :: MisoString)) 0.8
                    return NoOp
             Nothing ->
               put m { message = "Limite excedido: Maximo 5 fichas por casilla." }

  RemoveBet option -> do
    m <- get
    if spinning m
      then pure ()
      else
        let existingBets = activeBets m
        in case lookup option existingBets of
             Just chips ->
               if null chips
                 then pure ()
                 else
                   let chipToRemove = last chips
                       remainingChips = init chips
                       newBets = if null remainingChips
                                   then filter (\(o, _) -> o /= option) existingBets
                                   else (option, remainingChips) : filter (\(o, _) -> o /= option) existingBets
                       newModel = m { activeBets = newBets
                                    , balance = balance m + chipToRemove
                                    , message = "Ficha registrada."
                                    }
                   in newModel <# do
                        js_playSFX (unsafeCoerce ("sound-click" :: MisoString)) 0.8
                        return NoOp
             Nothing ->
               pure ()

  HoverBet opt -> do
    m <- get
    put m { hoveredOption = opt }

  ClearBets -> do
    m <- get
    if spinning m
      then pure ()
      else
        let refundedAmount = totalBetAmount m
            newModel = m { activeBets = []
                         , balance = balance m + refundedAmount
                         , message = "Apuestas retiradas de la mesa."
                         }
        in newModel <# do
             js_playSFX (unsafeCoerce ("sound-click" :: MisoString)) 0.8
             return NoOp

  Spin -> do
    m <- get
    if spinning m || null (activeBets m)
      then pure ()
      else
        m <# do
          winningNum <- getRandomInt 0 36
          return $ StartSpin winningNum

  StartSpin num -> do
    m <- get
    let nextMsg = "Hagan sus apuestas. Girando..."
        newModel = m
                     { spinning = True
                     , message = nextMsg
                     , winningTarget = Just num
                     }
    newModel <# do
      js_playSFX (unsafeCoerce ("sound-spin" :: MisoString)) 0.6
      threadDelay 768000
      return $ HandleResult num

  HandleResult num -> do
    m <- get
    let color = numberColor num
        newHistory = take 8 ((num, color) : history m)
        bets = activeBets m
        payout = sum [ calculatePayout opt (sum chips) num | (opt, chips) <- bets ]
        finalBalance = balance m + payout
        outcomeMsg = if payout > 0
          then "GANASTE $" ++ show payout ++ "! Cayo el numero " ++ show num ++ " (" ++ translateColor color ++ ")."
          else "Perdiste. Cayo el numero " ++ show num ++ " (" ++ translateColor color ++ ")."
        popup = if payout > 0 then Just payout else Nothing
        newModel = m
                     { spinning = False
                     , balance = finalBalance
                     , lastRoll = Just (num, color)
                     , history = newHistory
                     , message = outcomeMsg
                     , activeBets = []
                     , showWinPopup = popup
                     }
    newModel <# do
      if payout > 0
        then do
          js_triggerConfetti
          js_playSFX (unsafeCoerce ("sound-win" :: MisoString)) 0.7
          js_playVoice (unsafeCoerce (ms (speechPath num) :: MisoString))
          threadDelay 5000000
          return CloseWinPopup
        else do
          js_playSFX (unsafeCoerce ("sound-lose" :: MisoString)) 0.7
          js_playVoice (unsafeCoerce (ms (speechPath num) :: MisoString))
          return NoOp

-- | View representation
viewModel :: Model -> View Model Action
viewModel m =
  div_ [ class_ "container" ]
    [ -- Header section
      header_ []
        [ h1_ [] [ text "RULETA HASKELL" ]
        , button_ [ onClick ToggleMusic, class_ "btn-music" ]
            [ if musicPlaying m
                then text "Silenciar Musica"
                else text "Activar Musica"
            ]
        ]

      -- Stats section
    , div_ [ class_ "stats-grid" ]
        [ div_ [ class_ "stat-card" ]
            [ div_ [ class_ "stat-label" ] [ text "Tu Saldo" ]
            , div_ [ class_ "stat-val gold" ] [ text (ms ("$" ++ show (balance m))) ]
            ]
        , div_ [ class_ "stat-card" ]
            [ div_ [ class_ "stat-label" ] [ text "Apuesta Ficha" ]
            , div_ [ class_ "stat-val" ] [ text (ms ("$" ++ show (currentBetAmount m))) ]
            ]
        , div_ [ class_ "stat-card" ]
            [ div_ [ class_ "stat-label" ] [ text "Apuesta Total Mesa" ]
            , div_ [ class_ "stat-val gold" ] [ text (ms ("$" ++ show (totalBetAmount m))) ]
            ]
        ]

      -- Main Game area
    , div_ [ class_ "game-area glass-panel" ]
        [ -- Wheel panel (Left side - Large 3D Wheel)
          div_ [ class_ "wheel-container" ]
            [ div_ [ class_ "wheel-3d-wrapper", id_ "wheel-3d-wrapper", key_ (Key "wheel-3d-wrapper") ]
                [ div_ [ class_ "wheel-cabinet", id_ "wheel-cabinet", key_ (Key "wheel-cabinet") ]
                    [ div_ [ class_ "wheel-outer", id_ "wheel-outer", key_ (Key "wheel-outer") ]
                        [ -- Ball track is a static container now
                          div_ [ class_ "ball-track" ]
                            [ let oldNum = case lastRoll m of
                                             Just (n, _) -> n
                                             Nothing     -> 0
                                  newNum = case winningTarget m of
                                             Just n  -> n
                                             Nothing -> 0
                                  startAng = numberAngle oldNum
                                  targetAng = numberAngle newNum
                              in div_
                                   [ class_ (ms ("ball" ++ if spinning m then " ball-spinning" else ""))
                                   , Style.style_
                                       [ ("--start-angle", ms (show startAng ++ "deg"))
                                       , ("--target-angle", ms (show targetAng ++ "deg"))
                                       ]
                                   ]
                                   []
                            ]
                        ]
                    ]
                ]
            , div_ []
                [ case lastRoll m of
                    Just (num, col) ->
                      div_ [ class_ (ms ("wheel-number-display " ++ col)) ]
                        [ text (ms (show num)) ]
                    Nothing ->
                      div_ [ class_ "wheel-number-display", Style.style_ [("border", "2px dashed #585b70")] ]
                        [ text "-" ]
                ]
            , div_ [ class_ "history-container" ]
                (map (\(num, col) ->
                     div_ [ class_ (ms ("history-item " ++ col)) ] [ text (ms (show num)) ]
                     ) (history m))
            ]

          -- Betting board panel (Right side - Full Grid)
        , div_ [ class_ "board-container" ]
            [ renderBoard m

              -- Chips selector
            , div_ [ class_ "chips-container" ]
                [ chipButton 5 m
                , chipButton 10 m
                , chipButton 50 m
                , chipButton 100 m
                , chipButton 500 m
                ]

              -- Actions (Spin / Clear) row
            , div_ [ class_ "actions-row" ]
                [ button_
                    ([ class_ "btn-clear"
                     , onClick ClearBets
                     ] ++ if spinning m || null (activeBets m)
                          then [ disabled_ ]
                          else [])
                    [ text "Limpiar" ]
                , button_
                    ([ class_ "btn-spin"
                     , onClick Spin
                     ] ++ if spinning m || null (activeBets m)
                          then [ disabled_ ]
                          else [])
                    [ if spinning m then text "Girando..." else text "Girar Ruleta" ]
                ]

              -- Game message/alert box
            , div_ [ class_ "alert-box" ]
                [ text (ms (message m)) ]
            ]
        ]

      -- Footer section
    , footer_ []
        [ button_ [ onClick ResetGame, class_ "btn-reset" ] [ text "Reiniciar Saldo ($1000)" ]
        ]

      -- Static audio elements for the browser to control
    , audio_ [ id_ "sound-click", src_ "PonerFichas.mp3", preload_ "auto" ] []
    , audio_ [ id_ "sound-spin", src_ "BolitaGirando.mp3", preload_ "auto" ] []
    , audio_ [ id_ "sound-win", src_ "WinSFX.mp3", preload_ "auto" ] []
    , audio_ [ id_ "sound-lose", src_ "Failure.mp3", preload_ "auto" ] []
    , audio_ [ id_ "sound-music", src_ "CasinoMusic.mp3", loop_ True, preload_ "auto" ] []

      -- Win Popup Overlay
    , case showWinPopup m of
        Just payout ->
          div_ [ class_ "modal-overlay" ]
            [ div_ [ class_ "modal-content win-popup" ]
                [ h2_ [ class_ "modal-title" ] [ text "¡FELICIDADES!" ]
                , p_ [ class_ "modal-text" ] [ text "Has ganado" ]
                , div_ [ class_ "modal-amount" ] [ text (ms ("$" ++ show payout)) ]
                , button_ [ onClick CloseWinPopup, class_ "btn-modal-close" ] [ text "Continuar" ]
                ]
            ]
        Nothing ->
          div_ [] []

      -- Start Game Modal Overlay
    , if showStartModal m
        then
          div_ [ class_ "modal-overlay" ]
            [ div_ [ class_ "modal-content start-popup" ]
                [ h2_ [ class_ "modal-title start-title" ] [ text "¡RULETA HASKELL!" ]
                , p_ [ class_ "modal-text" ] [ text "Bienvenido al casino de Haskell WebAssembly. Pon a prueba tu suerte." ]
                , button_ [ onClick StartGame, class_ "btn-modal-start" ] [ text "Comenzar a Jugar" ]
                ]
            ]
        else
          div_ [] []
    ]

-- | Helper to build chip buttons
chipButton :: Int -> Model -> View Model Action
chipButton amt m =
  let isAffordable = balance m >= amt
      isActive = currentBetAmount m == amt
      cls = ms $ "chip chip-" ++ show amt
               ++ (if isActive then " active" else "")
               ++ (if not isAffordable then " disabled" else "")
      clickAttrs = if isAffordable && not (spinning m)
                     then [ onClick (SelectChip amt), draggable_ True ]
                     else [ draggable_ False ]
  in div_
       ([ class_ cls
        , prop "data-chip-value" (ms (show amt))
        ] ++ clickAttrs)
       [ text (ms (show amt)) ]

-- | Helper to render bet cell overlaying stack of chips if they exist and handling mouse events for hover highlights and right-click removals
renderCellWithChips :: BetOption -> MisoString -> [Attribute Action] -> [View Model Action] -> Model -> View Model Action
renderCellWithChips opt clsName extraAttrs content m =
  let chips = case lookup opt (activeBets m) of
                Just c  -> c
                Nothing -> []
      eventAttrs = [ class_ clsName
                   , onClick (PlaceBet opt)
                   , onContextMenu (RemoveBet opt)
                   , onMouseOver (HoverBet (Just opt))
                   , onMouseOut (HoverBet Nothing)
                   ]
  in div_
       (eventAttrs ++ extraAttrs)
       (content ++ if null chips then [] else [ renderChipsStack chips ])

-- | Renders overlapping 3D chip elements
renderChipsStack :: [Int] -> View Model Action
renderChipsStack chips =
  div_ [ class_ "chip-stack" ]
    [ div_
        [ class_ (ms ("placed-chip chip-" ++ show val))
        , Style.style_ [("transform", ms ("translateY(" ++ show (-2 * idx) ++ "px) translateZ(" ++ show (2 * idx) ++ "px)"))]
        ]
        [ text (ms (show val)) ]
    | (idx, val) <- zip [0..] chips
    ]

-- | Renders the complete traditional European Roulette betting layout
renderBoard :: Model -> View Model Action
renderBoard m =
  div_ [ class_ "betting-layout" ]
    ( [ -- Zero cell
        renderCellWithChips BetGreen
          (ms ("bet-cell zero"
            ++ if isSelected BetGreen then " selected" else ""
            ++ if isCellHighlighted BetGreen m then " hover-highlight" else ""
            ++ if isNumberCovered 0 m then " covered-highlight" else ""))
          []
          [ text "0" ]
          m
      ]
      ++
      -- 1 to 36 cells
      [ let highlight = if shouldHighlightNumber n (hoveredOption m) then " hover-highlight" else ""
            covered = if isNumberCovered n m then " covered-highlight" else ""
            selected = if isSelected (BetNumber n) then " selected" else ""
            cls = ms ("bet-cell " ++ numberColor n ++ selected ++ highlight ++ covered)
        in renderCellWithChips (BetNumber n)
             cls
             [ Style.style_
                 [ ("grid-column", ms (show (col n)))
                 , ("grid-row", ms (show (row n)))
                 ]
             ]
             [ text (ms (show n)) ]
             m
      | n <- [1..36]
      ]
      ++
      [ -- Dozens
        let highlight = if isCellHighlighted BetDoz1 m then " hover-highlight" else ""
            selected = if isSelected BetDoz1 then " selected" else ""
        in renderCellWithChips BetDoz1
             (ms ("bet-cell dozen" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "2 / span 4"), ("grid-row", "4")] ]
             [ text "1ra Docena", span_ [ class_ "bet-multiplier" ] [ text "2:1" ] ]
             m
      , let highlight = if isCellHighlighted BetDoz2 m then " hover-highlight" else ""
            selected = if isSelected BetDoz2 then " selected" else ""
        in renderCellWithChips BetDoz2
             (ms ("bet-cell dozen" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "6 / span 4"), ("grid-row", "4")] ]
             [ text "2da Docena", span_ [ class_ "bet-multiplier" ] [ text "2:1" ] ]
             m
      , let highlight = if isCellHighlighted BetDoz3 m then " hover-highlight" else ""
            selected = if isSelected BetDoz3 then " selected" else ""
        in renderCellWithChips BetDoz3
             (ms ("bet-cell dozen" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "10 / span 4"), ("grid-row", "4")] ]
             [ text "3ra Docena", span_ [ class_ "bet-multiplier" ] [ text "2:1" ] ]
             m

        -- Outside bets
      , let highlight = if isCellHighlighted BetLow m then " hover-highlight" else ""
            selected = if isSelected BetLow then " selected" else ""
        in renderCellWithChips BetLow
             (ms ("bet-cell outside" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "2 / span 2"), ("grid-row", "5")] ]
             [ text "1-18", span_ [ class_ "bet-multiplier" ] [ text "1:1" ] ]
             m
      , let highlight = if isCellHighlighted BetEven m then " hover-highlight" else ""
            selected = if isSelected BetEven then " selected" else ""
        in renderCellWithChips BetEven
             (ms ("bet-cell outside" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "4 / span 2"), ("grid-row", "5")] ]
             [ text "Par", span_ [ class_ "bet-multiplier" ] [ text "1:1" ] ]
             m
      , let highlight = if isCellHighlighted BetRed m then " hover-highlight" else ""
            selected = if isSelected BetRed then " selected" else ""
        in renderCellWithChips BetRed
             (ms ("bet-cell outside red" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "6 / span 2"), ("grid-row", "5")] ]
             [ text "Rojo", span_ [ class_ "bet-multiplier" ] [ text "1:1" ] ]
             m
      , let highlight = if isCellHighlighted BetBlack m then " hover-highlight" else ""
            selected = if isSelected BetBlack then " selected" else ""
        in renderCellWithChips BetBlack
             (ms ("bet-cell outside black" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "8 / span 2"), ("grid-row", "5")] ]
             [ text "Negro", span_ [ class_ "bet-multiplier" ] [ text "1:1" ] ]
             m
      , let highlight = if isCellHighlighted BetOdd m then " hover-highlight" else ""
            selected = if isSelected BetOdd then " selected" else ""
        in renderCellWithChips BetOdd
             (ms ("bet-cell outside" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "10 / span 2"), ("grid-row", "5")] ]
             [ text "Impar", span_ [ class_ "bet-multiplier" ] [ text "1:1" ] ]
             m
      , let highlight = if isCellHighlighted BetHigh m then " hover-highlight" else ""
            selected = if isSelected BetHigh then " selected" else ""
        in renderCellWithChips BetHigh
             (ms ("bet-cell outside" ++ selected ++ highlight))
             [ Style.style_ [("grid-column", "12 / span 2"), ("grid-row", "5")] ]
             [ text "19-36", span_ [ class_ "bet-multiplier" ] [ text "1:1" ] ]
             m
      ]
    )
  where
    col :: Int -> Int
    col n = (n - 1) `div` 3 + 2
    row :: Int -> Int
    row n = case n `mod` 3 of
              0 -> 1
              2 -> 2
              1 -> 3
              _ -> 1

    isSelected :: BetOption -> Bool
    isSelected opt = opt `elem` map fst (activeBets m)

-- | Helper to find the best affordable chip value
bestAffordableChip :: Int -> Int -> Int
bestAffordableChip bal current
  | bal >= current = current
  | otherwise =
      case filter (<= bal) [500, 100, 50, 10, 5] of
        (x:_) -> x
        []    -> 5

-- | Check if a number is included in a bet option
numberInBetOption :: Int -> BetOption -> Bool
numberInBetOption n = \case
  BetNumber num -> n == num
  BetGreen        -> n == 0
  BetRed          -> numberColor n == "red"
  BetBlack        -> numberColor n == "black"
  BetEven         -> n > 0 && n `mod` 2 == 0
  BetOdd          -> n `mod` 2 /= 0
  BetLow          -> n >= 1 && n <= 18
  BetHigh         -> n >= 19 && n <= 36
  BetDoz1         -> n >= 1 && n <= 12
  BetDoz2         -> n >= 13 && n <= 24
  BetDoz3         -> n >= 25 && n <= 36

-- | Check if a number is covered by any active bets
isNumberCovered :: Int -> Model -> Bool
isNumberCovered n m = any (\(opt, _) -> numberInBetOption n opt) (activeBets m)
