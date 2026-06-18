{-# LANGUAGE OverloadedStrings #-}
module Types where

import Miso
import Miso.Lens  

data Model = Model
  { _contador :: Int,
    _titulo :: MisoString
  } deriving (Show, Eq)


data Action
  = Restar 
  | Sumar
  | CambiarTitulo MisoString
  deriving (Show, Eq)

contador :: Lens Model Int
contador = lens _contador $ \record x -> record {_contador = x}

titulo :: Lens Model MisoString
titulo = lens _titulo $ \record x -> record {_titulo = x}

modeloInicial :: Model
modeloInicial = Model 0 "Título por defecto"
