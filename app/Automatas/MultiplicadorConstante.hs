{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , c
  , paginaActual
  , analizador
  ) where

import Miso
import Control.Lens
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H

import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Set as S
import SubAutomatas.InputValidado
import qualified UI.Math as UM
import qualified UI.Table as UT
import qualified Data.Vector as V
import SubAutomatas.AnalizadorEstadistico


-- | Modelo local para el método de Multiplicador Constante
data MultConstanteModel = MultConstanteModel
  { _inputSemilla         :: InputValidado
  , _inputIteraciones     :: InputValidado
  , _inputConstante       :: InputValidado
  , _parametrosOriginales :: Maybe (Int, Int) -- (Semilla, Constante)
  , _xn                   :: Int
  , _c                    :: Int 
  , _historial            :: [Int]
  , _paginaActual         :: Int
  , _analizador           :: AnalizadorModel
  } deriving (Show, Eq)

makeLenses ''MultConstanteModel

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
  | PaginaAnterior
  | PaginaSiguiente
  | AccionAnalizador AnalizadorAction
  deriving (Show, Eq)

-- | Estado inicial
xcero :: MultConstanteModel
xcero = MultConstanteModel (InputValidado "" Nothing)
                           (InputValidado "10" Nothing)
                           (InputValidado "" Nothing)
                           Nothing 0 0 [] 1 analizadorInicial


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
      , _paginaActual = 1
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
                     , _paginaActual = 1
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
         , _paginaActual = 1
         }

  Iterar ->
    let semillaActual = _xn modelo
        nuevoValor    = F.multiplicadorConstante (_c modelo) semillaActual
    in modelo
      { _xn        = nuevoValor
      , _historial = nuevoValor : _historial modelo
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



viewModel :: MultConstanteModel -> View model MultConstanteAction
viewModel modelo = H.div_ []
  [ H.h2_ [] 
      [ text "Generador: Multiplicador Constante "
      , H.span_ [ class_ "formula-title" ] [ UM.formulaMultiplicadorConstante ]
      ]
  
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
  , case _parametrosOriginales modelo of
      Nothing -> H.div_ [] []
      Just _  -> H.div_ []
         [ tablaHistorial (_paginaActual modelo) (_historial modelo)
         , if null (_historial modelo)
             then H.div_ [] []
             else H.div_ [ class_ "card fade-in" ]
               [ H.h3_ [] [ text "Pruebas Estadísticas" ]
               , H.button_ [ onClick (AccionAnalizador (EjecutarPruebas (V.fromList (map (realToFrac . F.pseudoaleatorioNC) (_historial modelo))))), class_ "btn-primary" ]
                   [ text "Ejecutar Pruebas Estadísticas" ]
               , H.hr_ []
               , fmap AccionAnalizador (viewAnalizador (_analizador modelo))
               ]
         ]
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
          , H.button_ [ onClick FijarParametros, class_ "btn-primary" ]
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

tablaHistorial :: Int -> [Int] -> View model MultConstanteAction
tablaHistorial pagAct historialList =
  UT.tablaPaginada
    [ UM.indexn, UM.xn, UM.rn ]
    historialList
    pagAct
    (\idx valor ->
       [ text (ms (show idx))
       , text (ms (show valor))
       , text (ms (show (F.pseudoaleatorioNC valor)))
       ]
    )
    PaginaAnterior
    PaginaSiguiente



