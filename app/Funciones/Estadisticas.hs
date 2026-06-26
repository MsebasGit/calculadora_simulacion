module Funciones.Estadisticas
  ( PruebaMediasResult (..)
  , PruebaVarianzasResult (..)
  , ChiCuadradaResult (..)
  , PruebaCorridasResult (..)
  , pruebaMedias
  , pruebaVarianzas
  , pruebaChiCuadrada
  , pruebaCorridas
  ) where

import Data.List (group)

-- | Resultado de la prueba de medias
data PruebaMediasResult = PruebaMediasResult
  { mediaMuestral :: Double
  , zCal          :: Double
  , zCritico      :: Double
  , pasaMedias    :: Bool
  } deriving (Show, Eq)

-- | Resultado de la prueba de varianzas
data PruebaVarianzasResult = PruebaVarianzasResult
  { varianzaMuestral :: Double
  , chiCalV          :: Double
  , chiLimInferior   :: Double
  , chiLimSuperior   :: Double
  , pasaVarianzas    :: Bool
  } deriving (Show, Eq)

-- | Resultado de la prueba de uniformidad (Chi-cuadrada)
data ChiCuadradaResult = ChiCuadradaResult
  { observados     :: [Int]
  , esperados      :: Double
  , chiCalU        :: Double
  , chiCriticoU    :: Double
  , pasaChi        :: Bool
  } deriving (Show, Eq)

-- | Resultado de la prueba de corridas (independencia)
data PruebaCorridasResult = PruebaCorridasResult
  { totalCorridas    :: Int
  , esperadoCorridas :: Double
  , varianzaCorridas :: Double
  , zCalCorridas     :: Double
  , zCriticoCorridas :: Double
  , pasaCorridas     :: Bool
  } deriving (Show, Eq)

-- | Prueba de Medias (H0: mu = 0.5)
pruebaMedias :: [Double] -> PruebaMediasResult
pruebaMedias xs =
  let n = fromIntegral (length xs)
      media = if n == 0 then 0 else sum xs / n
      z = (media - 0.5) * sqrt (12 * n)
      zCrit = 1.96 -- Para alpha = 0.05 (dos colas)
      pasa = n > 0 && abs z <= zCrit
  in PruebaMediasResult media z zCrit pasa

-- | Prueba de Varianzas (H0: sigma^2 = 1/12)
pruebaVarianzas :: [Double] -> PruebaVarianzasResult
pruebaVarianzas xs =
  let n = fromIntegral (length xs)
      media = if n == 0 then 0 else sum xs / n
      varianza = if n <= 1
                   then 0
                   else sum (map (\x -> (x - media) ^ (2 :: Int)) xs) / (n - 1)
      chi = if varianza == 0 then 0 else (n - 1) * varianza * 12.0
      -- Aproximación de Wilson-Hilferty para límites Chi-Cuadrada a alpha = 0.05
      df = n - 1
      -- Límite inferior (alpha/2 = 0.025, z = -1.96)
      limInf = if df <= 0 then 0 else df * (1 - 2/(9*df) - 1.96 * sqrt (2/(9*df))) ^ (3 :: Int)
      -- Límite superior (1 - alpha/2 = 0.975, z = 1.96)
      limSup = if df <= 0 then 0 else df * (1 - 2/(9*df) + 1.96 * sqrt (2/(9*df))) ^ (3 :: Int)
      pasa = df > 0 && chi >= limInf && chi <= limSup
  in PruebaVarianzasResult varianza chi limInf limSup pasa

-- | Prueba de Uniformidad (Chi-Cuadrada con k = 10 subintervalos)
pruebaChiCuadrada :: [Double] -> ChiCuadradaResult
pruebaChiCuadrada xs =
  let n = fromIntegral (length xs)
      k = 10.0
      ancho = 1.0 / k
      intervaloVal :: Double -> Int
      intervaloVal x = min 9 (floor (x / ancho))
      
      obsInit = replicate 10 0
      contar [] acc = acc
      contar (v:vs) acc =
        let idx = intervaloVal v
            acc' = take idx acc ++ [acc !! idx + 1] ++ drop (idx + 1) acc
        in contar vs acc'
        
      obs = contar xs obsInit
      esp = n / k
      chi = if n == 0 then 0 else sum [ (fromIntegral o - esp) ^ (2 :: Int) / esp | o <- obs ]
      -- Wilson-Hilferty para grados de libertad df = 9, alpha = 0.05 (z = 1.64485)
      df = 9.0
      chiCrit = df * (1 - 2/(9*df) + 1.64485 * sqrt (2/(9*df))) ^ (3 :: Int)
      pasa = n > 0 && chi <= chiCrit
  in ChiCuadradaResult obs esp chi chiCrit pasa

-- | Prueba de Corridas (Independencia con respecto a la media de 0.5)
pruebaCorridas :: [Double] -> PruebaCorridasResult
pruebaCorridas xs =
  let n = fromIntegral (length xs)
      clasificado = map (>= 0.5) xs
      n1 = fromIntegral (length (filter id clasificado))
      n2 = fromIntegral (length (filter not clasificado))
      runs = fromIntegral (length (group clasificado))
      
      mu = if n == 0 then 0 else (2 * n1 * n2) / n + 0.5
      varNum = 2 * n1 * n2 * (2 * n1 * n2 - n)
      varDen = n ^ (2 :: Int) * (n - 1)
      var = if varDen == 0 then 0 else varNum / varDen
      
      z = if var == 0 then 0 else (runs - mu) / sqrt var
      zCrit = 1.96 -- Para alpha = 0.05
      pasa = n1 > 0 && n2 > 0 && abs z <= zCrit
  in PruebaCorridasResult (round runs) mu var z zCrit pasa
