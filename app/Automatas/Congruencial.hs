{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.Congruencial
  ( CongruencialModel (..)
  , CongruencialAction (..)
  , xcero
  , updateModel
  , viewModel
  ) where

import Miso
import qualified Miso.Html as H
import Miso.Html.Property (class_)

-- | Modelo local para el método Congruencial
data CongruencialModel = CongruencialModel
  { _placeholderField :: MisoString
  } deriving (Show, Eq)

-- | Acciones locales para el método Congruencial
data CongruencialAction
  = CongruencialPlaceholderAction
  deriving (Show, Eq)

-- | Estado inicial
xcero :: CongruencialModel
xcero = CongruencialModel ""

-- | Actualización de estado local
updateModel :: CongruencialAction -> CongruencialModel -> CongruencialModel
updateModel = \case
  CongruencialPlaceholderAction -> id

-- | Vista local con modelo polimórfico
viewModel :: CongruencialModel -> View model CongruencialAction
viewModel _modelo =
  H.div_ [ class_ "card fade-in" ]
    [ H.h2_ [ class_ "card-title" ] [ text "Generador Congruencial" ]
    , H.p_ [ class_ "card-desc" ]
        [ text "Este módulo está planificado para contener el Generador Congruencial Lineal y Multiplicativo. Su implementación matemática se conectará con el módulo Funciones.Aleatorios." ]
    ]
