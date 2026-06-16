{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_calculadora_simulacion (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "calculadora_simulacion"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Aplicaci\243n que permite el uso de distintas t\233cnicas de simulaci\243n"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
