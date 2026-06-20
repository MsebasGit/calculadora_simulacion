{-# LANGUAGE OverloadedStrings #-}
module GlobalTypes
  ( Model (..)
  , Action (..)
  , Tab (..)
  , modeloInicial
  , cuadradosMedios
  , congruencial
  , pruebasEstadisticas
  , activeTab
  ) where

import Miso
import Miso.Lens
import qualified Automatas.CuadradosMedios as CM
import qualified Automatas.Congruencial as C
import qualified Automatas.PruebasEstadisticas as PE

-- | Modelo global
data Model = Model
  { _cuadradosMedios      :: CM.CuadradosMediosModel
  , _congruencial         :: C.CongruencialModel
  , _pruebasEstadisticas :: PE.PruebasEstadisticasModel
  , _activeTab            :: Tab
  } deriving (Show, Eq)

-- | Pestañas de navegación de la aplicación
data Tab
  = TabCuadradosMedios
  | TabCongruencial
  | TabPruebasEstadisticas
  deriving (Show, Eq)

-- | Acciones globales
data Action
  = AccionCuadradosMedios CM.CuadradosMediosAction
  | AccionCongruencial C.CongruencialAction
  | AccionPruebasEstadisticas PE.PruebasEstadisticasAction
  | CambiarTab Tab
  deriving (Show, Eq)

-- | Lentes globales
cuadradosMedios :: Lens Model CM.CuadradosMediosModel
cuadradosMedios = lens _cuadradosMedios $ \record x -> record {_cuadradosMedios = x}

congruencial :: Lens Model C.CongruencialModel
congruencial = lens _congruencial $ \record x -> record {_congruencial = x}

pruebasEstadisticas :: Lens Model PE.PruebasEstadisticasModel
pruebasEstadisticas = lens _pruebasEstadisticas $ \record x -> record {_pruebasEstadisticas = x}

activeTab :: Lens Model Tab
activeTab = lens _activeTab $ \record x -> record {_activeTab = x}

-- | Modelo inicial global
modeloInicial :: Model
modeloInicial = Model
  { _cuadradosMedios      = CM.xcero
  , _congruencial         = C.xcero
  , _pruebasEstadisticas = PE.xcero
  , _activeTab            = TabCuadradosMedios
  }
