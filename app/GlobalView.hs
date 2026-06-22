{-# LANGUAGE OverloadedStrings #-}
module GlobalView
  ( viewModel
  ) where

import Miso
import qualified Miso.Html as H
import GlobalTypes
import qualified Automatas.CuadradosMedios as CM
import qualified Automatas.Congruencial as C
import qualified Automatas.PruebasEstadisticas as PE
import qualified Automatas.MultiplicadorConstante as MC
import qualified Automatas.ProductosMedios as PM
import UI.Navbar (viewNavbar)
import Miso.Html.Property (class_)

-- | Renderizado principal de la interfaz global
viewModel :: () -> Model -> View Model Action
viewModel _ modelo = H.div_ [ class_ "app-container" ]
  [ H.h1_ [ class_ "app-title" ] [ text "Calculadora de Simulación (Autómata Principal)" ]
  , viewNavbar (_activeTab modelo)
  , H.hr_ [ class_ "divider" ]
  
  -- Enrutamiento visual según la pestaña activa dentro de una caja con animación fade-in:
  , H.main_ [ class_ "fade-in" ]
      [ case _activeTab modelo of
          TabCuadradosMedios ->
            fmap AccionCuadradosMedios (CM.viewModel (_cuadradosMedios modelo))
          TabCongruencial ->
            fmap AccionCongruencial (C.viewModel (_congruencial modelo))
          TabPruebasEstadisticas ->
            fmap AccionPruebasEstadisticas (PE.viewModel (_pruebasEstadisticas modelo))
          TabMultiplicadorConstante ->
            fmap AccionMultiplicadorConstante (MC.viewModel (_multiplicadorConstante modelo))
          TabProductosMedios ->
            fmap AccionProductosMedios (PM.viewModel (_productosMedios modelo))
      ]
  ]
