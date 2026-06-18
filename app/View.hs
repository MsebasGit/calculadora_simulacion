{-# LANGUAGE OverloadedStrings #-}

module View where

import Miso
import Miso.Html
import Miso.Html.Property
import Types -- Importamos el modelo y las acciones

viewModel :: () -> Model -> View Model Action
viewModel _props modelo = div_ []
  [ h1_ [] [ text (_titulo modelo) ] 
  , input_ [ type_ "text", onInput CambiarTitulo ] 
  , br_ []
  , button_ [ onClick Restar ] [ text "-" ]
  , text (ms (show (_contador modelo)))
  , button_ [ onClick Sumar ] [ text "+" ]
  ]
