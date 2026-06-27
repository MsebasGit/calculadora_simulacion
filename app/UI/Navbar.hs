{-# LANGUAGE OverloadedStrings #-}
module UI.Navbar
  ( viewMainNavbar
  , viewSubNavbar
  ) where

import Miso
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)
import GlobalTypes

-- | Navbar principal para las 3 secciones mayores de la aplicación
viewMainNavbar :: SeccionPrincipal -> View model Action
viewMainNavbar activeSecVal =
  H.nav_ [ class_ "navbar main-navbar" ]
    [ H.ul_ [ class_ "nav-list main-nav-list" ]
        [ navItem SeccionPseudoaleatorios "Números Pseudoaleatorios"
        , navItem SeccionRuleta "Ruleta de Casino"
        , navItem SeccionCovid "Autómata Celular COVID-19"
        ]
    ]
  where
    navItem sec label =
      let isActive = activeSecVal == sec
          itemClass = if isActive then "nav-item active" else "nav-item"
      in H.li_ []
           [ H.button_ [ onClick (CambiarSeccion sec), class_ itemClass ] [ text label ] ]

-- | Sub-navbar para seleccionar los distintos algoritmos de generación
viewSubNavbar :: Tab -> View model Action
viewSubNavbar activeTabVal =
  H.nav_ [ class_ "navbar sub-navbar" ]
    [ H.ul_ [ class_ "nav-list sub-nav-list" ]
        [ navItem TabCuadradosMedios "Cuadrados Medios"
        , navItem TabCongruencial "Congruencial Lineal"
        , navItem TabCongruencialMult "Congruencial Multiplicativo"
        , navItem TabMultiplicadorConstante "Multiplicador Constante"
        , navItem TabProductosMedios "Productos Medios"
        , navItem TabMersenneTwister "Mersenne Twister"
        ]
    ]
  where
    navItem tab label =
      let isActive = activeTabVal == tab
          itemClass = if isActive then "nav-item active" else "nav-item"
      in H.li_ []
           [ H.button_ [ onClick (CambiarTab tab), class_ itemClass ] [ text label ] ]
