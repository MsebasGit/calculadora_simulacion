{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Automatas.PruebasEstadisticas
  ( PruebasEstadisticasModel (..)
  , PruebasEstadisticasAction (..)
  , xcero
  , updateModel
  , viewModel
  , analizador
  , inputDatos
  ) where

import Miso
import Control.Lens
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Vector as V
import qualified SubAutomatas.AnalizadorEstadistico as AE
import SubAutomatas.InputValidado

-- | Modelo local para pruebas estadísticas
data PruebasEstadisticasModel = PruebasEstadisticasModel
  { _analizador :: AE.AnalizadorModel
  , _inputDatos :: InputValidado
  } deriving (Show, Eq)

makeLenses ''PruebasEstadisticasModel

-- | Acciones locales para pruebas estadísticas
data PruebasEstadisticasAction
  = AccionAnalizador AE.AnalizadorAction
  | AccionInputDatos InputValidadoAction
  | ProcesarDatos
  deriving (Show, Eq)

-- | Estado inicial
xcero :: PruebasEstadisticasModel
xcero = PruebasEstadisticasModel
  AE.analizadorInicial
  (InputValidado "" Nothing)

-- | Actualización de estado local
updateModel :: PruebasEstadisticasAction -> PruebasEstadisticasModel -> PruebasEstadisticasModel
updateModel action modelo = case action of
  AccionAnalizador subAct ->
    analizador %~ AE.updateAnalizador subAct $ modelo

  AccionInputDatos subAct ->
    inputDatos %~ updateInputValidado subAct $ modelo

  ProcesarDatos ->
    let texto = fromMisoString (_textoTemporal (_inputDatos modelo))
        -- Separar valores por comas o espacios
        valoresString = words (map (\c -> if c == ',' then ' ' else c) texto)
        valoresMaybe = map (readMaybe :: String -> Maybe Double) valoresString
    in if null valoresMaybe
       then inputDatos %~ (errorActual ?~ "No se ingresaron datos") $ modelo
       else case sequence valoresMaybe of
              Just valores ->
                let vec = V.fromList valores
                    modeloActualizado = modelo { _analizador = AE.updateAnalizador (AE.EjecutarPruebas vec) (_analizador modelo) }
                in inputDatos %~ (errorActual .~ Nothing) $ modeloActualizado
              Nothing ->
                inputDatos %~ (errorActual ?~ "Los datos deben ser números decimales válidos (ej: 0.12, 0.45)") $ modelo

-- | Vista local
viewModel :: PruebasEstadisticasModel -> View model PruebasEstadisticasAction
viewModel modelo = H.div_ [ class_ "card fade-in" ]
  [ H.h2_ [ class_ "card-title" ] [ text "Pruebas Estadísticas" ]
  , H.p_ [ class_ "card-desc" ]
      [ text "Ingrese una secuencia de números pseudoaleatorios (entre 0 y 1) separados por comas o espacios para realizar las pruebas de Medias, Varianza y Chi-Cuadrada." ]
  
  , H.div_ [ class_ "input-container" ]
      [ H.label_ [] [ text "Datos de entrada (ej. 0.12 0.54 0.89 0.23):" ]
      , viewInputValidado AccionInputDatos (_inputDatos modelo)
      , H.button_ [ onClick ProcesarDatos, class_ "btn-primary" ] [ text "Ejecutar Análisis" ]
      ]
  
  , H.hr_ []
  
  -- Renderizar el sub-autómata AnalizadorEstadistico
  , H.div_ []
      [ fmap AccionAnalizador (AE.viewAnalizador (_analizador modelo)) ]
  ]
