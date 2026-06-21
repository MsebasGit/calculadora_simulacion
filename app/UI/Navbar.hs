{-# LANGUAGE OverloadedStrings #-}
module UI.Navbar
  ( viewNavbar
  ) where

import Miso
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import GlobalTypes

-- | Navbar sin estilos (HTML puro) para navegar entre los autómatas
viewNavbar :: Tab -> View model Action
viewNavbar _activeTabVal =
  H.nav_ []
    [ H.ul_ []
        [ H.li_ []
            [ H.button_ [ onClick (CambiarTab TabCuadradosMedios) ] [ text "Cuadrados Medios" ]
            ]
        , H.li_ []
            [ H.button_ [ onClick (CambiarTab TabCongruencial) ] [ text "Congruencial" ]
            ]
        , H.li_ []
            [ H.button_ [ onClick (CambiarTab TabPruebasEstadisticas) ] [ text "Pruebas Estadísticas" ]
            ]
        , H.li_ []
            [ H.button_ [ onClick (CambiarTab TabMultiplicadorConstante) ] [ text "Multiplicador Constante" ]
            ]
        , H.li_ []
            [ H.button_ [ onClick (CambiarTab TabProductosMedios) ] [ text "Productos Medios" ]
            ]
        ]
    ]
