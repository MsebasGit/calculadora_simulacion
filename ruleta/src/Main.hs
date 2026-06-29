{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Miso
import qualified Data.Map as M
import Ruleta (initialModel, updateModel, viewModel)

#ifdef WASM
foreign export ccall hs_start :: IO ()
hs_start :: IO ()
hs_start = main
#endif

main :: IO ()
main = do
  let customEvents = M.insert "mouseover" BUBBLE $ M.insert "mouseout" BUBBLE defaultEvents
  startApp customEvents
    (component initialModel updateModel viewModel)
      { logLevel = DebugAll }
