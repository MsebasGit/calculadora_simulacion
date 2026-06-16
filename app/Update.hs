module Update where

import Miso
import Types

updateModel :: Action -> Model -> Effect Action Model
updateModel action (Model n) =
  case action of
    Increment -> noEff (Model (n + 1))
    Decrement -> noEff (Model (n - 1))
    NoOp -> noEff (Model n)
