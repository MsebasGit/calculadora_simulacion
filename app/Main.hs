{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Language.Javascript.JSaddle.Warp as JSaddle
import Types
import Update
import View

main :: IO ()
main = do
  putStrLn "Starting Miso application on http://localhost:3000..."
  JSaddle.run 3000 $ startApp App
    { model         = initialModel
    , update        = updateModel
    , view          = viewModel
    , events        = defaultEvents
    , subs          = []
    , initialAction = NoOp
    , mountPoint    = Nothing
    , logLevel      = Off
    }
