{-# LANGUAGE LambdaCase #-}
module GlobalUpdate
  ( updateModel
  ) where

import Miso
import Miso.Lens
import GlobalTypes
import qualified Automatas.CuadradosMedios as CM
import qualified Automatas.Congruencial as C
import qualified Automatas.PruebasEstadisticas as PE
import qualified Automatas.MultiplicadorConstante as MC
import qualified Automatas.ProductosMedios as PM

-- | Función de actualización global
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  AccionCuadradosMedios msg ->
    cuadradosMedios %= CM.updateModel msg

  AccionCongruencial msg ->
    congruencial %= C.updateModel msg

  AccionPruebasEstadisticas msg ->
    pruebasEstadisticas %= PE.updateModel msg

  AccionMultiplicadorConstante msg ->
    multiplicadorConstante %= MC.updateModel msg

  AccionProductosMedios msg ->
    productosMedios %= PM.updateModel msg

  CambiarTab tab ->
    activeTab .= tab
