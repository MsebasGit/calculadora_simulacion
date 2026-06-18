{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso

-- Importamos nuestros módulos locales
import Types   
import Update
import View

main :: IO ()
#ifdef WASM
main = startApp defaultEvents app

foreign export javascript "hs_start" main :: IO ()
#else
main = putStrLn "Para ejecutar la calculadora de simulación en el navegador, compila a WebAssembly usando: cabal build --target=wasm32-wasi"
#endif

app :: App Model Action
app = vcomp modeloInicial updateModel viewModel
