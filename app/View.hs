{-# LANGUAGE OverloadedStrings #-}
module View where

import Miso
import Miso.String (ms)
import Types

viewModel :: Model -> View Action
viewModel (Model n) =
  div_ []
    [ button_ [ onClick Decrement ] [ text "-" ]
    , span_ [] [ text (ms (show n)) ]
    , button_ [ onClick Increment ] [ text "+" ]
    ]
