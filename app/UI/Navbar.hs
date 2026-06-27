{-# LANGUAGE OverloadedStrings #-}
module UI.Navbar
  ( viewNavbar
  ) where

import Miso
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)
import GlobalTypes

-- | Navbar con estilos para navegar de forma amigable e interactiva
viewNavbar :: Tab -> View model Action
viewNavbar activeTabVal =
  H.nav_ [ class_ "navbar" ]
    [ H.ul_ [ class_ "nav-list" ]
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
