module Types where

import Miso
import Miso.String (MisoString)

data Model = Model Int
  deriving (Show, Eq)

initialModel :: Model
initialModel = Model 0

data Action
  = Increment
  | Decrement
  | NoOp
  deriving (Show, Eq)
