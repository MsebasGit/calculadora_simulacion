{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module GlobalUpdate
  ( updateModel
  ) where

import Miso
import Miso.Lens
import GlobalTypes
import qualified Automatas.CuadradosMedios as CM
import qualified Automatas.Congruencial as C
import qualified Automatas.CongruencialMult as CMul
import qualified Automatas.PruebasEstadisticas as PE
import qualified Automatas.MultiplicadorConstante as MC
import qualified Automatas.ProductosMedios as PM
import qualified Automatas.MersenneTwister as MT
import qualified Automatas.Ruleta as R
import Miso.Effect (Schedule(..))
import Control.Monad.RWS.Lazy (runRWS, ask, get, put, tell)

-- | Función de actualización global
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  AccionCuadradosMedios msg ->
    cuadradosMedios %= CM.updateModel msg

  AccionCongruencial msg ->
    congruencial %= C.updateModel msg

  AccionCongruencialMult msg ->
    congruencialMult %= CMul.updateModel msg

  AccionPruebasEstadisticas msg ->
    pruebasEstadisticas %= PE.updateModel msg

  AccionMultiplicadorConstante msg ->
    multiplicadorConstante %= MC.updateModel msg

  AccionProductosMedios msg ->
    productosMedios %= PM.updateModel msg

  AccionMersenneTwister msg ->
    mersenneTwister %= MT.updateModel msg

  AccionRuleta msg ->
    mapEffect ruleta AccionRuleta (R.updateModel msg)

  CambiarTab tab ->
    activeTab .= tab

  CambiarSeccion seccion ->
    seccionActiva .= seccion

mapSchedule :: (action1 -> action2) -> Schedule action1 -> Schedule action2
mapSchedule f (Schedule sync g) = Schedule sync (\sink2 -> g (sink2 . f))

mapEffect :: Lens Model R.Model
          -> (R.Action -> Action)
          -> Effect parent props R.Model R.Action
          -> Effect parent props Model Action
mapEffect l f eff = do
  r <- ask
  s2 <- get
  let s1 = _get l s2
      ((), s1', w1) = runRWS eff r s1
      s2' = _set l s1' s2
      w2 = map (mapSchedule f) w1
  put s2'
  tell w2
