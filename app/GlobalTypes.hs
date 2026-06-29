{-# LANGUAGE OverloadedStrings #-}
module GlobalTypes
  ( Model (..)
  , Action (..)
  , Tab (..)
  , modeloInicial
  , cuadradosMedios
  , congruencial
  , congruencialMult
  , pruebasEstadisticas
  , multiplicadorConstante
  , productosMedios
  , mersenneTwister
  , activeTab
  , SeccionPrincipal (..)
  , seccionActiva
  , ruleta
  ) where

import Miso.Lens
import qualified Automatas.CuadradosMedios as CM
import qualified Automatas.Congruencial as C
import qualified Automatas.CongruencialMult as CMul
import qualified Automatas.PruebasEstadisticas as PE
import qualified Automatas.MultiplicadorConstante as MC
import qualified Automatas.ProductosMedios as PM
import qualified Automatas.MersenneTwister as MT
import qualified Automatas.Ruleta as R

-- | Secciones principales de la aplicación
data SeccionPrincipal
  = SeccionPseudoaleatorios
  | SeccionRuleta
  | SeccionCovid
  deriving (Show, Eq)

-- | Modelo global
data Model = Model
  { _seccionActiva          :: SeccionPrincipal
  , _cuadradosMedios        :: CM.CuadradosMediosModel
  , _congruencial           :: C.CongruencialModel
  , _congruencialMult       :: CMul.CongruencialMultModel
  , _pruebasEstadisticas    :: PE.PruebasEstadisticasModel
  , _multiplicadorConstante :: MC.MultConstanteModel
  , _productosMedios        :: PM.ProductosMediosModel
  , _mersenneTwister        :: MT.MersenneTwisterModel
  , _ruleta                 :: R.Model
  , _activeTab              :: Tab
  } deriving (Show, Eq)

-- | Pestañas de navegación de la aplicación
data Tab
  = TabCuadradosMedios
  | TabCongruencial
  | TabCongruencialMult
  | TabPruebasEstadisticas
  | TabMultiplicadorConstante
  | TabProductosMedios
  | TabMersenneTwister
  deriving (Show, Eq)

-- | Acciones globales
data Action
  = AccionCuadradosMedios CM.CuadradosMediosAction
  | AccionCongruencial C.CongruencialAction
  | AccionCongruencialMult CMul.CongruencialMultAction
  | AccionPruebasEstadisticas PE.PruebasEstadisticasAction
  | AccionMultiplicadorConstante MC.MultConstanteAction
  | AccionProductosMedios PM.ProductosMediosAction
  | AccionMersenneTwister MT.MersenneTwisterAction
  | AccionRuleta R.Action
  | CambiarTab Tab
  | CambiarSeccion SeccionPrincipal
  deriving (Show, Eq)

-- | Lentes globales
cuadradosMedios :: Lens Model CM.CuadradosMediosModel
cuadradosMedios = lens _cuadradosMedios $ \record x -> record {_cuadradosMedios = x}

congruencial :: Lens Model C.CongruencialModel
congruencial = lens _congruencial $ \record x -> record {_congruencial = x}

congruencialMult :: Lens Model CMul.CongruencialMultModel
congruencialMult = lens _congruencialMult $ \record x -> record {_congruencialMult = x}

pruebasEstadisticas :: Lens Model PE.PruebasEstadisticasModel
pruebasEstadisticas = lens _pruebasEstadisticas $ \record x -> record {_pruebasEstadisticas = x}

multiplicadorConstante :: Lens Model MC.MultConstanteModel
multiplicadorConstante = lens _multiplicadorConstante $ \record x -> record {_multiplicadorConstante = x}

productosMedios :: Lens Model PM.ProductosMediosModel
productosMedios = lens _productosMedios $ \record x -> record {_productosMedios = x}

mersenneTwister :: Lens Model MT.MersenneTwisterModel
mersenneTwister = lens _mersenneTwister $ \record x -> record {_mersenneTwister = x}

activeTab :: Lens Model Tab
activeTab = lens _activeTab $ \record x -> record {_activeTab = x}

seccionActiva :: Lens Model SeccionPrincipal
seccionActiva = lens _seccionActiva $ \record x -> record {_seccionActiva = x}

ruleta :: Lens Model R.Model
ruleta = lens _ruleta $ \record x -> record {_ruleta = x}

-- | Modelo inicial global
modeloInicial :: Model
modeloInicial = Model
  { _seccionActiva          = SeccionPseudoaleatorios
  , _cuadradosMedios        = CM.xcero
  , _congruencial           = C.xcero
  , _congruencialMult       = CMul.xcero
  , _pruebasEstadisticas    = PE.xcero
  , _multiplicadorConstante = MC.xcero
  , _productosMedios        = PM.xcero
  , _mersenneTwister        = MT.xcero
  , _ruleta                 = R.initialModel
  , _activeTab              = TabCuadradosMedios
  }
