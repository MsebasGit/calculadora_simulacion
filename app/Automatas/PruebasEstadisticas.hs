{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.PruebasEstadisticas
  ( PruebasEstadisticasModel (..)
  , PruebasEstadisticasAction (..)
  , xcero
  , updateModel
  , viewModel
  ) where

import Miso
import qualified Miso.Html as H
import Miso.Html.Property (class_)

-- | Modelo local para pruebas estadísticas
data PruebasEstadisticasModel = PruebasEstadisticasModel
  { _placeholderField :: MisoString
  } deriving (Show, Eq)

-- | Acciones locales para pruebas estadísticas
data PruebasEstadisticasAction
  = PruebasEstadisticasPlaceholderAction
  deriving (Show, Eq)

-- | Estado inicial
xcero :: PruebasEstadisticasModel
xcero = PruebasEstadisticasModel ""

-- | Actualización de estado local
updateModel :: PruebasEstadisticasAction -> PruebasEstadisticasModel -> PruebasEstadisticasModel
updateModel = \case
  PruebasEstadisticasPlaceholderAction -> id

-- | Vista local con modelo polimórfico
viewModel :: PruebasEstadisticasModel -> View model PruebasEstadisticasAction
viewModel _modelo =
  H.div_ [ class_ "card fade-in" ]
    [ H.h2_ [ class_ "card-title" ] [ text "Pruebas Estadísticas" ]
    , H.p_ [ class_ "card-desc" ]
        [ text "Este módulo contendrá la lógica de validación estadística (Pruebas de Medias, Varianzas, Uniformidad e Independencia) conectándose con el módulo Funciones.Estadisticas." ]
    ]
