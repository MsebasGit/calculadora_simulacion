{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.MultiplicadorConstante  
  ( MultConstanteModel (..)
  , MultConstanteAction (..)
  , xcero
  , updateModel
  , viewModel
  , xn
  , inputSemilla
  , inputIteraciones
  , inputConstante
  , parametrosOriginales
  , historial
  ) where

import Miso
import Miso.Lens
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H

import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Set as S
import SubAutomatas.InputValidado
import qualified UI.Math as UM


-- | Modelo local para el método de Multiplicador Constante
data MultConstanteModel = MultConstanteModel
  { _inputSemilla         :: InputValidado
  , _inputIteraciones     :: InputValidado
  , _inputConstante       :: InputValidado
  , _parametrosOriginales :: Maybe (Int, Int) -- (Semilla, Constante)
  , _xn                   :: Int
  , _c                    :: Int 
  , _historial            :: [Int]
  } deriving (Show, Eq)

-- | Acciones locales para el método de Multiplicador Constante
data MultConstanteAction 
  = AccionInputSemilla InputValidadoAction
  | AccionInputIteraciones InputValidadoAction
  | AccionInputConstante InputValidadoAction
  | FijarParametros             -- Acción para fijar ambos parámetros a la vez
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  deriving (Show, Eq)

-- | Lentes para manipular el modelo local
xn :: Lens MultConstanteModel Int
xn = lens _xn $ \record x -> record {_xn = x}

inputSemilla :: Lens MultConstanteModel InputValidado
inputSemilla = lens _inputSemilla $ \record x -> record {_inputSemilla = x}

inputIteraciones :: Lens MultConstanteModel InputValidado
inputIteraciones = lens _inputIteraciones $ \record x -> record {_inputIteraciones = x}

inputConstante :: Lens MultConstanteModel InputValidado
inputConstante = lens _inputConstante $ \record x -> record {_inputConstante = x}

parametrosOriginales :: Lens MultConstanteModel (Maybe (Int, Int))
parametrosOriginales = lens _parametrosOriginales $ \record x -> record {_parametrosOriginales = x}

historial :: Lens MultConstanteModel [Int]
historial = lens _historial $ \record x -> record {_historial = x}

-- | Estado inicial
xcero :: MultConstanteModel
xcero = MultConstanteModel (InputValidado "" Nothing)
                           (InputValidado "10" Nothing)
                           (InputValidado "" Nothing)
                           Nothing 0 0 [] 


-- | Actualización de estado local (pure update)
updateModel :: MultConstanteAction -> MultConstanteModel -> MultConstanteModel
updateModel action modelo = case action of
  AccionInputSemilla subAct ->
    inputSemilla %~ updateInputValidado subAct $ modelo

  AccionInputIteraciones subAct ->
    inputIteraciones %~ updateInputValidado subAct $ modelo

  AccionInputConstante subAct ->
    inputConstante %~ updateInputValidado subAct $ modelo

  FijarParametros ->
    let strSemilla = fromMisoString (_textoTemporal (_inputSemilla modelo))
        strConstante = fromMisoString (_textoTemporal (_inputConstante modelo))
        valSemilla = readMaybe strSemilla
        valConstante = readMaybe strConstante
    in case (valSemilla, valConstante) of
         (Just s, Just c)
           | s >= 100 && c > 0 ->
               let modificado = modelo
                     { _xn = s
                     , _c = c
                     , _parametrosOriginales = Just (s, c)
                     }
                   semLimpia = inputSemilla %~ (errorActual .~ Nothing) $ modificado
               in inputConstante %~ (errorActual .~ Nothing) $ semLimpia
           | otherwise ->
               let semErr = if s >= 100
                              then inputSemilla %~ (errorActual .~ Nothing) $ modelo
                              else inputSemilla %~ (errorActual ?~ "La semilla debe ser mayor o igual a 100") $ modelo
                   constErr = if c > 0
                                then inputConstante %~ (errorActual .~ Nothing) $ semErr
                                else inputConstante %~ (errorActual ?~ "La constante debe ser mayor a 0") $ semErr
               in constErr
         (mS, mC) ->
           let semErr = case mS of
                          Just s | s >= 100 -> inputSemilla %~ (errorActual .~ Nothing) $ modelo
                                 | otherwise -> inputSemilla %~ (errorActual ?~ "La semilla debe ser mayor o igual a 100") $ modelo
                          Nothing -> inputSemilla %~ (errorActual ?~ "La semilla debe ser un número entero válido") $ modelo
               constErr = case mC of
                            Just c | c > 0 -> inputConstante %~ (errorActual .~ Nothing) $ semErr
                                   | otherwise -> inputConstante %~ (errorActual ?~ "La constante debe ser mayor a 0") $ semErr
                            Nothing -> inputConstante %~ (errorActual ?~ "La constante debe ser un número entero válido") $ semErr
           in constErr
        
  Reiniciar ->
    modelo
      { _parametrosOriginales = Nothing
      , _xn = 0
      , _c = 0
      , _historial = []
      }

  IterarNUsuario ->
    let str = fromMisoString (_textoTemporal (_inputIteraciones modelo))
    in case readMaybe str of
         Just n
           | n > 0 ->
               let semillaActual = _xn modelo
                   nuevosValores = drop 1 $ take (n + 1) $ iterate (F.multiplicadorConstante (_c modelo)) semillaActual
                   modeloConHistorial = modelo
                     { _xn        = if null nuevosValores then semillaActual else last nuevosValores
                     , _historial = reverse nuevosValores ++ _historial modelo
                     }
               in inputIteraciones %~ (errorActual .~ Nothing) $ modeloConHistorial
           | otherwise ->
               inputIteraciones %~ (errorActual ?~ "Debe ingresar al menos 1 iteración") $ modelo
         Nothing ->
           inputIteraciones %~ (errorActual ?~ "Ingrese un número entero válido") $ modelo

  IterarN n ->
    let semillaActual = _xn modelo
        nuevosValores = drop 1 $ take (n + 1) $ iterate (F.multiplicadorConstante (_c modelo)) semillaActual
    in if n <= 0 then modelo
       else modelo
         { _xn        = if null nuevosValores then semillaActual else last nuevosValores
         , _historial = reverse nuevosValores ++ _historial modelo
         }

  Iterar ->
    let semillaActual = _xn modelo
        nuevoValor    = F.multiplicadorConstante (_c modelo) semillaActual
    in modelo
      { _xn        = nuevoValor
      , _historial = nuevoValor : _historial modelo
      }


viewModel :: MultConstanteModel -> View model MultConstanteAction
viewModel modelo = H.div_ [ ]
  [ H.h2_ [] [ text "Generador: Multiplicador Constante" ]
  
  -- Mostramos el estado actual del generador
  , H.div_ [ ] 
      [ H.strong_ [] [ text "Semilla original (", UM.x0, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (s, _) -> ms (show s))
      ]
  , H.div_ [ ] 
      [ H.strong_ [] [ text "Constante (", UM.constc, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (_, c) -> ms (show c))
      ]
  , H.div_ [ ] 
      [ H.strong_ [] [ text "Valor actual (", UM.xn, text "): " ]
      , text (ms (show (_xn modelo))) 
      ]
  
  -- Separamos los controles y la tabla en funciones más pequeñas
  , panelControles modelo
  , H.hr_ []
  , tablaHistorial (_historial modelo)
  ]

---
--- COMPONENTES DE LA INTERFAZ
---

panelControles :: MultConstanteModel -> View model MultConstanteAction
panelControles modelo = H.div_ [ ]
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
          , H.div_ [] [ text "Constante (", UM.constc, text "):" ]
          , viewInputValidado AccionInputConstante (_inputConstante modelo)
          , H.hr_ []
          , H.button_ [ onClick FijarParametros ]
                      [ text "Fijar Semilla y Constante" ]
          ]

-- | Retorna True si el historial contiene elementos duplicados
tieneDuplicados :: [Int] -> Bool
tieneDuplicados lista =
  length lista /= S.size (S.fromList lista)

-- | Renderiza los controles de simulación (si cicloDetectado es True, deshabilita las iteraciones)
controlesSimulacion :: Bool -> MultConstanteModel -> View model MultConstanteAction
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

tablaHistorial :: [Int] -> View model MultConstanteAction
tablaHistorial historialList = 
  H.table_ []
    [ H.thead_ []
      [ H.tr_ []
        [ H.th_ [] [ UM.indexn ]
        , H.th_ [] [ UM.xn ]
        , H.th_ [] [ UM.rn ]
        ]
      ]
    , H.tbody_ [] filasHTML
    ]
  where
    listaValores = reverse historialList
    listaPseudo  = map F.pseudoaleatorioNC listaValores
    
    listaNumerada = zip3 [1..] listaValores listaPseudo :: [(Int, Int, Float)]
    
    filasHTML = [ H.tr_ [] 
                    [ H.td_ [  ] 
                        [ text (ms (show iteracion)) ]
                    , H.td_ [  ] 
                        [ text (ms (show valor)) ] 
                    , H.td_ [  ] 
                        [ text (ms (show pseudo)) ] 
                    ] 
                | (iteracion, valor, pseudo) <- listaNumerada 
                ]                

