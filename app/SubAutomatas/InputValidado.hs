{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module SubAutomatas.InputValidado
  ( InputValidado (..)
  , InputValidadoAction (..)
  , textoTemporal
  , errorActual
  , updateInputValidado
  , viewInputValidado
  ) where

import Miso
import Control.Lens
import qualified Miso.Html as H
import Miso.Html.Event (onInput)
import Miso.Html.Property (type_, value_, class_)

-- | Sub-autómata para gestionar entradas de texto con validaciones
data InputValidado = InputValidado
  { _textoTemporal :: MisoString
  , _errorActual   :: Maybe MisoString
  } deriving (Show, Eq)

makeLenses ''InputValidado

-- | Acciones locales para el sub-autómata
data InputValidadoAction
  = Escribir MisoString
  | MostrarError MisoString
  | LimpiarError
  deriving (Show, Eq)

-- | Actualización del estado
updateInputValidado :: InputValidadoAction -> InputValidado -> InputValidado
updateInputValidado = \case
  Escribir texto -> \modelo -> modelo { _textoTemporal = texto }
  MostrarError err -> \modelo -> modelo { _errorActual = Just err }
  LimpiarError -> \modelo -> modelo { _errorActual = Nothing }

-- | Vista reutilizable del sub-autómata
viewInputValidado :: (InputValidadoAction -> action) -> InputValidado -> View model action
viewInputValidado toParentAction modelo =
  H.div_ [ class_ "input-container" ]
    [ H.input_
        [ type_ "text"
        , value_ (_textoTemporal modelo)
        , onInput (toParentAction . Escribir)
        , class_ "input-field"
        ]
    , case _errorActual modelo of
        Nothing -> H.div_ [] []
        Just err -> H.div_ [ class_ "error-message" ] [ text err ]
    ]
