{-# LANGUAGE LambdaCase        #-}
module Update where

import Miso
import Miso.Lens 
import Types

updateModel :: Action -> Effect parent props Model Action
updateModel = \case  
    Sumar                     -> contador += 1 
    Restar                    -> contador -= 1
    CambiarTitulo nuevoTitulo -> titulo   .= nuevoTitulo 
