{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_jsaddle (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "jsaddle"
version :: Version
version = Version [0,9,9,4] []

synopsis :: String
synopsis = "Interface for JavaScript that works with GHCJS and GHC"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
