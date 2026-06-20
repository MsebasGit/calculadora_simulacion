{-# LANGUAGE OverloadedStrings #-}
module GlobalView
  ( viewModel
  ) where

import Miso
import qualified Miso.Html as H
import GlobalTypes
import qualified Automatas.CuadradosMedios as CM

-- | Renderizado principal de la interfaz global
viewModel :: () -> Model -> View Model Action
viewModel _ modelo = H.div_ [ ]
  [ H.h1_ [] [ text "Calculadora de Simulación (Autómata Principal)" ]
  , H.hr_ []
  
  -- Aquí ocurre la magia del enrutamiento visual:
  , fmap AccionCuadradosMedios (CM.viewModel (_cuadradosMedios modelo))
  ]
