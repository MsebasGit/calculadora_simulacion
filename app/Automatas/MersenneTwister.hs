{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Automatas.MersenneTwister
  ( MersenneTwisterModel (..)
  , MersenneTwisterAction (..)
  , xcero
  , updateModel
  , viewModel
  , inputSemilla
  , inputIteraciones
  , semillaOriginal
  , mtState
  , mtIndex
  , historial
  , paginaActual
  , analizador
  ) where

import Miso
import Control.Lens
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import Text.Printf (printf)
import SubAutomatas.InputValidado
import SubAutomatas.AnalizadorEstadistico
import qualified UI.Math as UM
import qualified UI.Table as UT
import Data.Word (Word32)
import Data.Bits (xor, shiftR, shiftL, (.&.), (.|.))
import qualified Data.Vector as V

-- | Modelo local para el método de Mersenne Twister
data MersenneTwisterModel = MersenneTwisterModel
  { _inputSemilla      :: InputValidado
  , _inputIteraciones  :: InputValidado
  , _semillaOriginal   :: Maybe Word32
  , _mtState           :: !(V.Vector Word32)
  , _mtIndex           :: !Int
  , _historial         :: [(Word32, Double)] -- (Y_n, R_n)
  , _paginaActual      :: Int
  , _analizador        :: AnalizadorModel
  } deriving (Show, Eq)

makeLenses ''MersenneTwisterModel

-- | Acciones locales para Mersenne Twister
data MersenneTwisterAction
  = AccionInputSemilla InputValidadoAction
  | AccionInputIteraciones InputValidadoAction
  | FijarSemilla
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  | PaginaAnterior
  | PaginaSiguiente
  | AccionAnalizador AnalizadorAction
  deriving (Show, Eq)

-- | Estado inicial vacío
xcero :: MersenneTwisterModel
xcero = MersenneTwisterModel
  (InputValidado "" Nothing)
  (InputValidado "10" Nothing)
  Nothing
  V.empty
  624
  []
  1
  analizadorInicial

-- | Inicialización de Mersenne Twister a partir de una semilla
initMT :: Word32 -> V.Vector Word32
initMT seed = V.fromList (initLoop 1 seed [seed])
  where
    initLoop :: Int -> Word32 -> [Word32] -> [Word32]
    initLoop i _ acc | i >= 624 = reverse acc
    initLoop i prev acc =
      let val = 1812433253 * (prev `xor` (prev `shiftR` 30)) + fromIntegral i
      in initLoop (i + 1) val (val : acc)

-- | Twist del estado de Mersenne Twister
twist :: V.Vector Word32 -> V.Vector Word32
twist v = V.generate 624 (\i ->
  let y = (v V.! i .&. 0x80000000) .|. (v V.! ((i + 1) `mod` 624) .&. 0x7FFFFFFF)
      val = (v V.! ((i + 397) `mod` 624)) `xor` (y `shiftR` 1)
  in if y .&. 1 /= 0
       then val `xor` 0x9908B0DF
       else val
  )

-- | Avanzar un paso en el Mersenne Twister
nextMTVal :: (V.Vector Word32, Int) -> (Word32, (V.Vector Word32, Int))
nextMTVal (stateVec, idx) =
  let (stateVec', idx') = if idx >= 624
                            then (twist stateVec, 0)
                            else (stateVec, idx)
      y = stateVec' V.! idx'
      y1 = y `xor` (y `shiftR` 11)
      y2 = y1 `xor` ((y1 `shiftL` 7) .&. 0x9D2C5680)
      y3 = y2 `xor` ((y2 `shiftL` 15) .&. 0xEFC60000)
      y4 = y3 `xor` (y3 `shiftR` 18)
  in (y4, (stateVec', idx' + 1))

-- | Generar múltiples valores a partir de un estado
generarMT :: Int -> (V.Vector Word32, Int) -> ([(Word32, Double)], (V.Vector Word32, Int))
generarMT n initial = genLoop n initial []
  where
    genLoop :: Int -> (V.Vector Word32, Int) -> [(Word32, Double)] -> ([(Word32, Double)], (V.Vector Word32, Int))
    genLoop 0 st acc = (reverse acc, st)
    genLoop k st acc =
      let (y, st') = nextMTVal st
          r = fromIntegral y / 4294967296.0
      in genLoop (k - 1) st' ((y, r) : acc)

-- | Actualización de estado local (pure update)
updateModel :: MersenneTwisterAction -> MersenneTwisterModel -> MersenneTwisterModel
updateModel action modelo = case action of
  AccionInputSemilla subAct ->
    inputSemilla %~ updateInputValidado subAct $ modelo

  AccionInputIteraciones subAct ->
    inputIteraciones %~ updateInputValidado subAct $ modelo

  FijarSemilla ->
    let str = fromMisoString (_textoTemporal (_inputSemilla modelo))
    in case readMaybe str of
         Just nuevaSemilla ->
           let stateVec = initMT nuevaSemilla
               modificado = modelo
                 { _mtState = stateVec
                 , _mtIndex = 624
                 , _semillaOriginal = Just nuevaSemilla
                 , _historial = []
                 , _paginaActual = 1
                 }
           in inputSemilla %~ (errorActual .~ Nothing) $ modificado
         Nothing ->
           inputSemilla %~ (errorActual ?~ "La semilla debe ser un número entero de 32 bits válido (0 a 4294967295)") $ modelo

  Reiniciar ->
    case _semillaOriginal modelo of
      Nothing -> modelo
      Just _  -> modelo
        { _semillaOriginal = Nothing
        , _mtState = V.empty
        , _mtIndex = 624
        , _historial = []
        , _paginaActual = 1
        }

  IterarNUsuario ->
    let str = fromMisoString (_textoTemporal (_inputIteraciones modelo))
    in case readMaybe str of
         Just n
           | n > 0 ->
               let (nuevosValores, (stateVec', idx')) = generarMT n (_mtState modelo, _mtIndex modelo)
                   modeloConHistorial = modelo
                     { _mtState = stateVec'
                     , _mtIndex = idx'
                     , _historial = reverse nuevosValores ++ _historial modelo
                     , _paginaActual = 1
                     }
               in inputIteraciones %~ (errorActual .~ Nothing) $ modeloConHistorial
           | otherwise ->
               inputIteraciones %~ (errorActual ?~ "Debe ingresar al menos 1 iteración") $ modelo
         Nothing ->
           inputIteraciones %~ (errorActual ?~ "Ingrese un número entero válido") $ modelo

  IterarN n ->
    if n <= 0 then modelo
    else
      let (nuevosValores, (stateVec', idx')) = generarMT n (_mtState modelo, _mtIndex modelo)
      in modelo
        { _mtState = stateVec'
        , _mtIndex = idx'
        , _historial = reverse nuevosValores ++ _historial modelo
        , _paginaActual = 1
        }

  Iterar ->
    let (y, (stateVec', idx')) = nextMTVal (_mtState modelo, _mtIndex modelo)
        r = fromIntegral y / 4294967296.0
    in modelo
      { _mtState = stateVec'
      , _mtIndex = idx'
      , _historial = (y, r) : _historial modelo
      , _paginaActual = 1
      }

  PaginaAnterior ->
    if _paginaActual modelo > 1
      then modelo { _paginaActual = _paginaActual modelo - 1 }
      else modelo

  PaginaSiguiente ->
    let totalElementos = length (_historial modelo)
        maxPagina = UT.calcularMaxPagina totalElementos
    in if _paginaActual modelo < maxPagina
         then modelo { _paginaActual = _paginaActual modelo + 1 }
         else modelo

  AccionAnalizador subAct ->
    analizador %~ updateAnalizador subAct $ modelo

viewModel :: MersenneTwisterModel -> View model MersenneTwisterAction
viewModel modelo = H.div_ []
  [ H.h2_ []
      [ text "Generador Mersenne Twister (MT19937) "
      , H.span_ [ class_ "formula-title" ] [ UM.formulaMersenneTwister ]
      ]

  , H.div_ []
      [ H.strong_ [] [ text "Semilla original (", UM.x0, text "): " ]
      , text (case _semillaOriginal modelo of
                 Nothing -> "No establecida"
                 Just s0 -> ms (show s0))
      ]
  , H.div_ []
      [ H.strong_ [] [ text "Índice de estado actual: " ]
      , text (ms (show (_mtIndex modelo)))
      ]

  , panelControles modelo
  , H.hr_ []
  , case _semillaOriginal modelo of
      Nothing -> H.div_ [] []
      Just _  -> H.div_ []
         [ tablaHistorial (_paginaActual modelo) (_historial modelo)
         , if null (_historial modelo)
             then H.div_ [] []
             else H.div_ [ class_ "card fade-in" ]
               [ H.h3_ [] [ text "Pruebas Estadísticas" ]
               , H.button_ [ onClick (AccionAnalizador (EjecutarPruebas (V.fromList (map snd (_historial modelo))))), class_ "btn-primary" ]
                   [ text "Ejecutar Pruebas Estadísticas" ]
               , H.hr_ []
               , fmap AccionAnalizador (viewAnalizador (_analizador modelo))
               ]
         ]
  ]

panelControles :: MersenneTwisterModel -> View model MersenneTwisterAction
panelControles modelo = H.div_ []
  [ mostrarControles ]
  where
    mostrarControles =
      case _semillaOriginal modelo of
        Nothing -> H.div_ []
          [ H.div_ [] [ text "Semilla (", UM.x0, text "):" ]
          , viewInputValidado AccionInputSemilla (_inputSemilla modelo)
          , H.button_ [ onClick FijarSemilla, class_ "btn-primary" ]
                      [ text ("Pulse aquí para fijar la semilla: " <> _textoTemporal (_inputSemilla modelo))]
          ]
        Just _  -> controlesSimulacion modelo

controlesSimulacion :: MersenneTwisterModel -> View model MersenneTwisterAction
controlesSimulacion modelo = H.div_ []
  [ H.button_ [ onClick Iterar ] [ text "Iterar 1 vez" ]
  , H.button_ [ onClick (IterarN 10) ] [ text "Iterar 10 veces" ]
  , H.hr_ []
  , H.div_ [] [ text "Cantidad personalizada de iteraciones:" ]
  , viewInputValidado AccionInputIteraciones (_inputIteraciones modelo)
  , H.button_ [ onClick IterarNUsuario ]
              [ text ("Iterar " <> _textoTemporal (_inputIteraciones modelo) <> " veces") ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar y cambiar semilla" ]
  ]

tablaHistorial :: Int -> [(Word32, Double)] -> View model MersenneTwisterAction
tablaHistorial pagAct historialList =
  UT.tablaPaginada
    [ UM.indexn, H.span_ [] [ text "Yn (Word32)" ], UM.rn ]
    historialList
    pagAct
    (\idx (yVal, rVal) ->
       [ text (ms (show idx))
       , text (ms (show yVal))
       , text (ms (printf "%.4f" rVal :: String))
       ]
    )
    PaginaAnterior
    PaginaSiguiente
