{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.Congruencial
  ( CongruencialModel (..)
  , CongruencialAction (..)
  , xcero
  , updateModel
  , viewModel
  , xn
  , inputSemilla
  , inputMultiplicador
  , inputConstante
  , inputModulo
  , inputIteraciones
  , parametrosOriginales
  , historial
  ) where

import Miso
import Miso.Lens
import qualified Data.Vector as V
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Set as S
import SubAutomatas.InputValidado
import qualified UI.Math as UM


-- | Modelo local para el método Congruencial Lineal
data CongruencialModel = CongruencialModel
  { _inputSemilla         :: InputValidado
  , _inputMultiplicador   :: InputValidado
  , _inputConstante       :: InputValidado
  , _inputModulo          :: InputValidado
  , _inputIteraciones     :: InputValidado
  , _parametrosOriginales :: Maybe (Int, Int, Int, Int) -- (x0, a, c, m)
  , _xn                   :: Int
  , _a                    :: Int
  , _c                    :: Int
  , _m                    :: Int
  , _historial            :: V.Vector Int
  } deriving (Show, Eq)

-- | Acciones locales para el método Congruencial Lineal
data CongruencialAction
  = AccionInputSemilla InputValidadoAction
  | AccionInputMultiplicador InputValidadoAction
  | AccionInputConstante InputValidadoAction
  | AccionInputModulo InputValidadoAction
  | AccionInputIteraciones InputValidadoAction
  | FijarParametros
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  deriving (Show, Eq)

-- | Lentes para manipular el modelo local
xn :: Lens CongruencialModel Int
xn = lens _xn $ \record x -> record {_xn = x}

inputSemilla :: Lens CongruencialModel InputValidado
inputSemilla = lens _inputSemilla $ \record x -> record {_inputSemilla = x}

inputMultiplicador :: Lens CongruencialModel InputValidado
inputMultiplicador = lens _inputMultiplicador $ \record x -> record {_inputMultiplicador = x}

inputConstante :: Lens CongruencialModel InputValidado
inputConstante = lens _inputConstante $ \record x -> record {_inputConstante = x}

inputModulo :: Lens CongruencialModel InputValidado
inputModulo = lens _inputModulo $ \record x -> record {_inputModulo = x}

inputIteraciones :: Lens CongruencialModel InputValidado
inputIteraciones = lens _inputIteraciones $ \record x -> record {_inputIteraciones = x}

parametrosOriginales :: Lens CongruencialModel (Maybe (Int, Int, Int, Int))
parametrosOriginales = lens _parametrosOriginales $ \record x -> record {_parametrosOriginales = x}

historial :: Lens CongruencialModel (V.Vector Int)
historial = lens _historial $ \record x -> record {_historial = x}

-- | Estado inicial
xcero :: CongruencialModel
xcero = CongruencialModel
  (InputValidado "" Nothing)
  (InputValidado "" Nothing)
  (InputValidado "" Nothing)
  (InputValidado "" Nothing)
  (InputValidado "10" Nothing)
  Nothing 0 0 0 0 V.empty

-- | Actualización de estado local (pure update)
updateModel :: CongruencialAction -> CongruencialModel -> CongruencialModel
updateModel action modelo = case action of
  AccionInputSemilla subAct ->
    inputSemilla %~ updateInputValidado subAct $ modelo

  AccionInputMultiplicador subAct ->
    inputMultiplicador %~ updateInputValidado subAct $ modelo

  AccionInputConstante subAct ->
    inputConstante %~ updateInputValidado subAct $ modelo

  AccionInputModulo subAct ->
    inputModulo %~ updateInputValidado subAct $ modelo

  AccionInputIteraciones subAct ->
    inputIteraciones %~ updateInputValidado subAct $ modelo

  FijarParametros ->
    let vX0 = readMaybe (fromMisoString (_textoTemporal (_inputSemilla modelo)))
        vA  = readMaybe (fromMisoString (_textoTemporal (_inputMultiplicador modelo)))
        vC  = readMaybe (fromMisoString (_textoTemporal (_inputConstante modelo)))
        vM  = readMaybe (fromMisoString (_textoTemporal (_inputModulo modelo)))
    in case (vX0, vA, vC, vM) of
         (Just x0, Just a, Just c, Just m)
           | x0 >= 0 && a > 0 && c >= 0 && m > x0 ->
               let modificado = modelo
                     { _xn = x0
                     , _a = a
                     , _c = c
                     , _m = m
                     , _parametrosOriginales = Just (x0, a, c, m)
                     }
                   l0 = inputSemilla %~ (errorActual .~ Nothing) $ modificado
                   l1 = inputMultiplicador %~ (errorActual .~ Nothing) $ l0
                   l2 = inputConstante %~ (errorActual .~ Nothing) $ l1
               in inputModulo %~ (errorActual .~ Nothing) $ l2
           | otherwise ->
               let e0 = if x0 >= 0
                          then inputSemilla %~ (errorActual .~ Nothing) $ modelo
                          else inputSemilla %~ (errorActual ?~ "La semilla debe ser mayor o igual a 0") $ modelo
                   e1 = if a > 0
                          then inputMultiplicador %~ (errorActual .~ Nothing) $ e0
                          else inputMultiplicador %~ (errorActual ?~ "El multiplicador (a) debe ser mayor a 0") $ e0
                   e2 = if c >= 0
                          then inputConstante %~ (errorActual .~ Nothing) $ e1
                          else inputConstante %~ (errorActual ?~ "La constante (c) debe ser mayor o igual a 0") $ e1
                   e3 = if m > x0
                          then inputModulo %~ (errorActual .~ Nothing) $ e2
                          else inputModulo %~ (errorActual ?~ "El módulo (m) debe ser mayor que la semilla") $ e2
               in e3
         (mX0, mA, mC, mM) ->
           let e0 = case mX0 of
                      Just x0 | x0 >= 0 -> inputSemilla %~ (errorActual .~ Nothing) $ modelo
                              | otherwise -> inputSemilla %~ (errorActual ?~ "La semilla debe ser >= 0") $ modelo
                      Nothing -> inputSemilla %~ (errorActual ?~ "Ingrese un entero válido") $ modelo
               e1 = case mA of
                      Just a | a > 0 -> inputMultiplicador %~ (errorActual .~ Nothing) $ e0
                             | otherwise -> inputMultiplicador %~ (errorActual ?~ "El multiplicador (a) debe ser > 0") $ e0
                      Nothing -> inputMultiplicador %~ (errorActual ?~ "Ingrese un entero válido") $ e0
               e2 = case mC of
                      Just c | c >= 0 -> inputConstante %~ (errorActual .~ Nothing) $ e1
                             | otherwise -> inputConstante %~ (errorActual ?~ "La constante (c) debe ser >= 0") $ e1
                      Nothing -> inputConstante %~ (errorActual ?~ "Ingrese un entero válido") $ e1
               e3 = case mM of
                      Just m | m > 0 -> inputModulo %~ (errorActual .~ Nothing) $ e2
                             | otherwise -> inputModulo %~ (errorActual ?~ "El módulo (m) debe ser > 0") $ e2
                      Nothing -> inputModulo %~ (errorActual ?~ "Ingrese un entero válido") $ e2
           in e3

  Reiniciar ->
    modelo
      { _parametrosOriginales = Nothing
      , _xn = 0
      , _a = 0
      , _c = 0
      , _m = 0
      , _historial = V.empty
      }

  IterarNUsuario ->
    let str = fromMisoString (_textoTemporal (_inputIteraciones modelo))
    in case readMaybe str of
         Just n
           | n > 0 ->
               let semillaActual = _xn modelo
                   nuevosValores = V.tail $ V.iterateN (n + 1) (\x -> F.congruencialLineal x (_a modelo) (_c modelo) (_m modelo)) semillaActual
                   modeloConHistorial = modelo
                     { _xn        = if V.null nuevosValores then semillaActual else V.last nuevosValores
                     , _historial = _historial modelo V.++ nuevosValores
                     }
               in inputIteraciones %~ (errorActual .~ Nothing) $ modeloConHistorial
           | otherwise ->
               inputIteraciones %~ (errorActual ?~ "Debe ingresar al menos 1 iteración") $ modelo
         Nothing ->
           inputIteraciones %~ (errorActual ?~ "Ingrese un número entero válido") $ modelo

  IterarN n ->
    let semillaActual = _xn modelo
        nuevosValores = V.tail $ V.iterateN (n + 1) (\x -> F.congruencialLineal x (_a modelo) (_c modelo) (_m modelo)) semillaActual
    in if n <= 0 then modelo
       else modelo
         { _xn        = if V.null nuevosValores then semillaActual else V.last nuevosValores
         , _historial = _historial modelo V.++ nuevosValores
         }

  Iterar ->
    let semillaActual = _xn modelo
        nuevoValor    = F.congruencialLineal semillaActual (_a modelo) (_c modelo) (_m modelo)
    in modelo
      { _xn        = nuevoValor
      , _historial = _historial modelo `V.snoc` nuevoValor
      }

-- | Renderizado visual del autómata Congruencial Lineal
viewModel :: CongruencialModel -> View model CongruencialAction
viewModel modelo = H.div_ []
  [ H.h2_ [] [ text "Generador Congruencial Lineal" ]
  
  , H.div_ [] 
      [ H.strong_ [] [ text "Semilla original (", UM.x0, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (x0Val, _, _, _) -> ms (show x0Val))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Multiplicador (", UM.consta, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, aVal, _, _) -> ms (show aVal))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Constante aditiva (", UM.constc, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (_, _, cVal, _) -> ms (show cVal))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Módulo (", UM.constm, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecido"
                 Just (_, _, _, mVal) -> ms (show mVal))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Valor actual (", UM.xn, text "): " ]
      , text (ms (show (_xn modelo))) 
      ]
  
  , panelControles modelo
  , H.hr_ []
  , tablaHistorial (_historial modelo)
  ]

panelControles :: CongruencialModel -> View model CongruencialAction
panelControles modelo = H.div_ []
  [ mostrarControles ]
  where
    mostrarControles =
      case _parametrosOriginales modelo of
        Just _ -> 
          let cicloDetectado = tieneDuplicados (_historial modelo)
          in controlesSimulacion cicloDetectado modelo
        Nothing -> H.div_ []
          [ H.div_ [] [ text "Semilla (", UM.x0, text "):" ]
          , viewInputValidado AccionInputSemilla (_inputSemilla modelo)
          , H.hr_ []
          , H.div_ [] [ text "Multiplicador (", UM.consta, text "):" ]
          , viewInputValidado AccionInputMultiplicador (_inputMultiplicador modelo)
          , H.hr_ []
          , H.div_ [] [ text "Constante aditiva (", UM.constc, text "):" ]
          , viewInputValidado AccionInputConstante (_inputConstante modelo)
          , H.hr_ []
          , H.div_ [] [ text "Módulo (", UM.constm, text "):" ]
          , viewInputValidado AccionInputModulo (_inputModulo modelo)
          , H.hr_ []
          , H.button_ [ onClick FijarParametros ]
                      [ text "Fijar Parámetros" ]
          ]

tieneDuplicados :: V.Vector Int -> Bool
tieneDuplicados vec =
  let lista = V.toList vec
  in length lista /= S.size (S.fromList lista)

controlesSimulacion :: Bool -> CongruencialModel -> View model CongruencialAction
controlesSimulacion True _ = H.div_ []
  [ H.div_ [ class_ "warning-message" ] 
           [ text "¡Ciclo detectado! La secuencia ha comenzado a repetirse. Simulación detenida." ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar / Cambiar parámetros" ]
  ]
controlesSimulacion False modelo = H.div_ []
  [ H.button_ [ onClick Iterar ] [ text "Iterar 1 vez" ]
  , H.button_ [ onClick (IterarN 10) ] [ text "Iterar 10 veces" ]
  , H.hr_ []
  , H.div_ [] [ text "Cantidad personalizada de iteraciones:" ]
  , viewInputValidado AccionInputIteraciones (_inputIteraciones modelo)
  , H.button_ [ onClick IterarNUsuario ]
              [ text ("Iterar " <> _textoTemporal (_inputIteraciones modelo) <> " veces") ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar / Cambiar parámetros" ]
  ]

tablaHistorial :: V.Vector Int -> View model CongruencialAction
tablaHistorial historialVec = 
  H.table_ []
    [ H.thead_ []
      [ H.tr_ []
        [ H.th_ [] [ UM.indexn ]
        , H.th_ [] [ UM.xn ]
        ]
      ]
    , H.tbody_ [] filasHTML
    ]
  where
    listaValores = V.toList historialVec
    listaNumerada = zip [1..] listaValores :: [(Int, Int)]
    filasHTML = [ H.tr_ [] 
                    [ H.td_ [] [ text (ms (show iteracion)) ]
                    , H.td_ [] [ text (ms (show valor)) ] 
                    ] 
                | (iteracion, valor) <- listaNumerada 
                ]
