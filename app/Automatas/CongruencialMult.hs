{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Automatas.CongruencialMult
  ( CongruencialMultModel (..)
  , CongruencialMultAction (..)
  , xcero
  , updateModel
  , viewModel
  , xn
  , inputSemilla
  , inputK
  , inputG
  , opt
  , inputIteraciones
  , parametrosOriginales
  , historial
  , paginaActual
  , analizador
  ) where

import Miso
import Control.Lens
import qualified Data.Vector as V
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Set as S
import SubAutomatas.InputValidado
import SubAutomatas.AnalizadorEstadistico
import qualified UI.Math as UM
import qualified UI.Table as UT
import Text.Printf (printf)

-- | Modelo local para el método Congruencial Multiplicativo
data CongruencialMultModel = CongruencialMultModel
  { _inputSemilla         :: InputValidado
  , _inputK               :: InputValidado
  , _inputG               :: InputValidado
  , _opt                  :: Int -- 3 o 5, por defecto 3 o 5
  , _inputIteraciones     :: InputValidado
  , _parametrosOriginales :: Maybe (Int, Int, Int, Int) -- (x0, k, g, opt)
  , _xn                   :: Int
  , _kVal                 :: Int
  , _gVal                 :: Int
  , _optVal               :: Int
  , _historial            :: V.Vector Int
  , _paginaActual         :: Int
  , _analizador           :: AnalizadorModel
  } deriving (Show, Eq)

makeLenses ''CongruencialMultModel

-- | Acciones locales para el método Congruencial Multiplicativo
data CongruencialMultAction
  = AccionInputSemilla InputValidadoAction
  | AccionInputK InputValidadoAction
  | AccionInputG InputValidadoAction
  | SeleccionarOpt Int
  | AccionInputIteraciones InputValidadoAction
  | FijarParametros
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  | PaginaAnterior
  | PaginaSiguiente
  | AccionAnalizador AnalizadorAction
  deriving (Show, Eq)

-- | Estado inicial del autómata congruencial multiplicativo
xcero :: CongruencialMultModel
xcero = CongruencialMultModel
  (InputValidado "" Nothing)
  (InputValidado "" Nothing)
  (InputValidado "" Nothing)
  3
  (InputValidado "10" Nothing)
  Nothing 0 0 0 0 V.empty 1 analizadorInicial

-- | Actualización de estado local (pure update)
updateModel :: CongruencialMultAction -> CongruencialMultModel -> CongruencialMultModel
updateModel action modelo = case action of
  AccionInputSemilla subAct ->
    inputSemilla %~ updateInputValidado subAct $ modelo

  AccionInputK subAct ->
    inputK %~ updateInputValidado subAct $ modelo

  AccionInputG subAct ->
    inputG %~ updateInputValidado subAct $ modelo

  SeleccionarOpt valor ->
    modelo { _opt = valor }

  AccionInputIteraciones subAct ->
    inputIteraciones %~ updateInputValidado subAct $ modelo

  FijarParametros ->
    let vX0 = readMaybe (fromMisoString (_textoTemporal (_inputSemilla modelo)))
        vK  = readMaybe (fromMisoString (_textoTemporal (_inputK modelo)))
        vG  = readMaybe (fromMisoString (_textoTemporal (_inputG modelo)))
        vOpt = _opt modelo
    in case (vX0, vK, vG) of
         (Just x0, Just k, Just g)
           | x0 > 0 && odd x0 && k >= 0 && g >= 2 && x0 < 2^g ->
               let modificado = modelo
                     { _xn = x0
                     , _kVal = k
                     , _gVal = g
                     , _optVal = vOpt
                     , _parametrosOriginales = Just (x0, k, g, vOpt)
                     }
                   l0 = inputSemilla %~ (errorActual .~ Nothing) $ modificado
                   l1 = inputK %~ (errorActual .~ Nothing) $ l0
               in inputG %~ (errorActual .~ Nothing) $ l1
           | otherwise ->
               let e0 = if x0 > 0 && odd x0
                          then inputSemilla %~ (errorActual .~ Nothing) $ modelo
                          else if x0 <= 0 
                            then inputSemilla %~ (errorActual ?~ "La semilla debe ser mayor a 0") $ modelo
                            else inputSemilla %~ (errorActual ?~ "La semilla debe ser impar") $ modelo
                   e1 = if k >= 0
                          then inputK %~ (errorActual .~ Nothing) $ e0
                          else inputK %~ (errorActual ?~ "El valor k debe ser mayor o igual a 0") $ e0
                   e2 = if g >= 2
                          then inputG %~ (errorActual .~ Nothing) $ e1
                          else inputG %~ (errorActual ?~ "El valor g debe ser mayor o igual a 2 (m = 2^g)") $ e1
                   e3 = if (not (x0 > 0 && odd x0) || not (g >= 2) || x0 < 2^g)
                          then e2
                          else inputSemilla %~ (errorActual ?~ "La semilla debe ser menor que el modulo (2^g)") $ e2
               in e3
         (mX0, mK, mG) ->
           let e0 = case mX0 of
                      Just x0 | x0 > 0 && odd x0 -> inputSemilla %~ (errorActual .~ Nothing) $ modelo
                              | x0 <= 0 -> inputSemilla %~ (errorActual ?~ "La semilla debe ser > 0") $ modelo
                              | otherwise -> inputSemilla %~ (errorActual ?~ "La semilla debe ser impar") $ modelo
                      Nothing -> inputSemilla %~ (errorActual ?~ "Ingrese un entero válido") $ modelo
               e1 = case mK of
                      Just k | k >= 0 -> inputK %~ (errorActual .~ Nothing) $ e0
                             | otherwise -> inputK %~ (errorActual ?~ "El valor k debe ser >= 0") $ e0
                      Nothing -> inputK %~ (errorActual ?~ "Ingrese un entero válido") $ e0
               e2 = case mG of
                      Just g | g >= 2 -> inputG %~ (errorActual .~ Nothing) $ e1
                             | otherwise -> inputG %~ (errorActual ?~ "El valor g debe ser >= 2") $ e1
                      Nothing -> inputG %~ (errorActual ?~ "Ingrese un entero válido") $ e1
               e3 = case (mX0, mG) of
                      (Just x0, Just g) | x0 >= 2^g && x0 > 0 && odd x0 && g >= 2 ->
                        inputSemilla %~ (errorActual ?~ "La semilla debe ser menor que el modulo (2^g)") $ e2
                      _ -> e2
           in e3

  Reiniciar ->
    modelo
      { _parametrosOriginales = Nothing
      , _xn = 0
      , _kVal = 0
      , _gVal = 0
      , _optVal = 3
      , _historial = V.empty
      , _paginaActual = 1
      }

  IterarNUsuario ->
    let str = fromMisoString (_textoTemporal (_inputIteraciones modelo))
    in case readMaybe str of
         Just n
           | n > 0 ->
               let semillaActual = _xn modelo
                   nuevosValores = V.tail $ V.iterateN (n + 1) (\x -> F.congLinealMult x (_kVal modelo) (_gVal modelo) (_optVal modelo)) semillaActual
                   modeloConHistorial = modelo
                     { _xn        = if V.null nuevosValores then semillaActual else V.last nuevosValores
                     , _historial = _historial modelo V.++ nuevosValores
                     , _paginaActual = 1
                     }
               in inputIteraciones %~ (errorActual .~ Nothing) $ modeloConHistorial
           | otherwise ->
               inputIteraciones %~ (errorActual ?~ "Debe ingresar al menos 1 iteración") $ modelo
         Nothing ->
           inputIteraciones %~ (errorActual ?~ "Ingrese un número entero válido") $ modelo

  IterarN n ->
    let semillaActual = _xn modelo
        nuevosValores = V.tail $ V.iterateN (n + 1) (\x -> F.congLinealMult x (_kVal modelo) (_gVal modelo) (_optVal modelo)) semillaActual
    in if n <= 0 then modelo
       else modelo
         { _xn        = if V.null nuevosValores then semillaActual else V.last nuevosValores
         , _historial = _historial modelo V.++ nuevosValores
         , _paginaActual = 1
         }

  Iterar ->
    let semillaActual = _xn modelo
        nuevoValor    = F.congLinealMult semillaActual (_kVal modelo) (_gVal modelo) (_optVal modelo)
    in modelo
      { _xn        = nuevoValor
      , _historial = _historial modelo `V.snoc` nuevoValor
      , _paginaActual = 1
      }

  PaginaAnterior ->
    if _paginaActual modelo > 1
      then modelo { _paginaActual = _paginaActual modelo - 1 }
      else modelo

  PaginaSiguiente ->
    let totalElementos = V.length (_historial modelo)
        maxPagina = UT.calcularMaxPagina totalElementos
    in if _paginaActual modelo < maxPagina
         then modelo { _paginaActual = _paginaActual modelo + 1 }
         else modelo

  AccionAnalizador subAct ->
    analizador %~ updateAnalizador subAct $ modelo

-- | Renderizado visual del autómata Congruencial Multiplicativo
viewModel :: CongruencialMultModel -> View model CongruencialMultAction
viewModel modelo = H.div_ []
  [ H.h2_ [] 
      [ text "Generador Congruencial Multiplicativo "
      , H.span_ [ class_ "formula-title" ] [ UM.formulaCongruencialMult ]
      ]
  
  , H.div_ [] 
      [ H.strong_ [] [ text "Semilla original (", UM.x0, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (x0Val, _, _, _) -> ms (show x0Val))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Parámetro k (", UM.constk, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, kO, _, _) -> ms (show kO))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Parámetro g (", UM.constg, text " donde ", UM.formulaM2g, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, _, gO, _) -> ms (show gO))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Multiplicador (", UM.consta, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, kO, _, optO) -> ms (show (optO + 8 * kO) ++ " (" ++ show optO ++ " + 8*" ++ show kO ++ ")"))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Módulo (", UM.formulaM2g, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, _, gO, _) -> ms (show (2^gO :: Int) ++ " (2^" ++ show gO ++ ")"))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Periodo Máximo (", UM.formulaPeriodoN, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, _, gO, _) -> ms (show (2^(gO - 2) :: Int) ++ " (2^" ++ show (gO - 2) ++ ")"))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Valor actual (", UM.xn, text "): " ]
      , text (ms (show (_xn modelo))) 
      ]
  
  , panelControles modelo
  , H.hr_ []
  , case _parametrosOriginales modelo of
      Nothing -> H.div_ [] []
      Just _  -> H.div_ []
         [ tablaHistorial (_paginaActual modelo) (2 ^ _gVal modelo) (_historial modelo)
         , if V.null (_historial modelo)
             then H.div_ [] []
             else H.div_ [ class_ "card fade-in" ]
               [ H.h3_ [] [ text "Pruebas Estadísticas" ]
               , H.button_ [ onClick (AccionAnalizador (EjecutarPruebas (V.map (\x -> F.pseudoaleatorioCL x (2 ^ _gVal modelo)) (_historial modelo)))), class_ "btn-primary" ]
                   [ text "Ejecutar Pruebas Estadísticas" ]
               , H.hr_ []
               , fmap AccionAnalizador (viewAnalizador (_analizador modelo))
               ]
         ]
  ]

panelControles :: CongruencialMultModel -> View model CongruencialMultAction
panelControles modelo = H.div_ []
  [ mostrarControles ]
  where
    mostrarControles =
      case _parametrosOriginales modelo of
        Just _ -> 
          let cicloDetectado = tieneDuplicados (_historial modelo)
          in controlesSimulacion cicloDetectado modelo
        Nothing -> H.div_ []
          [ H.div_ [] [ text "Semilla impar (", UM.x0, text "):" ]
          , viewInputValidado AccionInputSemilla (_inputSemilla modelo)
          , H.hr_ []
          , H.div_ [] [ text "Parámetro ", UM.constk, text " (", UM.consta, text " = opt + 8", UM.constk, text "):" ]
          , viewInputValidado AccionInputK (_inputK modelo)
          , H.hr_ []
          , H.div_ [] [ text "Parámetro ", UM.constg, text " (", UM.formulaM2g, text "):" ]
          , viewInputValidado AccionInputG (_inputG modelo)
          , H.hr_ []
          , H.div_ [] 
              [ H.span_ [] [ text "Fórmula del multiplicador: " ]
              , H.button_ 
                  [ onClick (SeleccionarOpt 3)
                  , class_ (if _opt modelo == 3 then "btn-primary" else "btn-secondary")
                  ] 
                  [ text "a = 3 + 8k" ]
              , H.span_ [] [ text " " ]
              , H.button_ 
                  [ onClick (SeleccionarOpt 5)
                  , class_ (if _opt modelo == 5 then "btn-primary" else "btn-secondary")
                  ] 
                  [ text "a = 5 + 8k" ]
              ]
          , H.hr_ []
          , H.button_ [ onClick FijarParametros, class_ "btn-primary" ]
                      [ text "Fijar Parámetros" ]
          ]

tieneDuplicados :: V.Vector Int -> Bool
tieneDuplicados vec =
  let lista = V.toList vec
  in length lista /= S.size (S.fromList lista)

controlesSimulacion :: Bool -> CongruencialMultModel -> View model CongruencialMultAction
controlesSimulacion True _ = H.div_ []
  [ H.div_ [ class_ "warning-message" ] 
           [ text "¡Ciclo detectado! La secuencia ha comenzado a repetirse. Simulación detenida." ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar / Cambiar parámetros" ]
  ]
controlesSimulacion False modelo = H.div_ []
  [ H.button_ [ onClick Iterar, class_ "btn-primary" ] [ text "Iterar 1 vez" ]
  , H.button_ [ onClick (IterarN 10), class_ "btn-secondary" ] [ text "Iterar 10 veces" ]
  , H.hr_ []
  , H.div_ [] [ text "Cantidad personalizada de iteraciones:" ]
  , viewInputValidado AccionInputIteraciones (_inputIteraciones modelo)
  , H.button_ [ onClick IterarNUsuario, class_ "btn-secondary" ]
              [ text ("Iterar " <> _textoTemporal (_inputIteraciones modelo) <> " veces") ]
  , H.hr_ []
  , H.button_ [ onClick Reiniciar, class_ "btn-danger" ] [ text "Reiniciar / Cambiar parámetros" ]
  ]

tablaHistorial :: Int -> Int -> V.Vector Int -> View model CongruencialMultAction
tablaHistorial pagAct mVal historialVec = 
  UT.tablaPaginada
    [ UM.indexn, UM.xn, UM.rn ]
    (V.toList historialVec)
    pagAct
    (\idx valor ->
       [ text (ms (show idx))
       , text (ms (show valor))
       , text (ms (printf "%.4f" (F.pseudoaleatorioCL valor mVal) :: String))
       ]
    )
    PaginaAnterior
    PaginaSiguiente
