module Funciones.Aleatorios
  ( congruencialLineal
  , congLinealMult
  , cuadradosMedios
  , productosMedios
  , multiplicadorConstante
  , pseudoaleatorioNC
  , pseudoaleatorioCL
  ) where

-- | Generador Congruencial Lineal
congruencialLineal :: Int -> Int -> Int -> Int -> Int
congruencialLineal x0 a c m = (a * x0 + c) `mod` m

-- | Obtiene los dígitos centrales de un número (para cuadrados medios, etc.)
centro :: Int -> Int
centro n = (n `div` 100) `mod` 10000

-- | Algoritmo de Cuadrados Medios
cuadradosMedios :: Int -> Int
cuadradosMedios x0 = centro $ x0 ^ (2 :: Int)

-- | Algoritmo de Productos Medios
productosMedios :: (Int, Int) -> (Int, Int)
productosMedios (x0, x1) = (x1, centro (x0 * x1))

-- | Algoritmo de Multiplicador Constante
multiplicadorConstante :: Int -> Int -> Int
multiplicadorConstante c x0 = centro $ x0 * c

-- | Generar pseudoaleatorio de 4 decimales entre 0 y 1 (No congruencial)
pseudoaleatorioNC :: Int -> Float
pseudoaleatorioNC n = fromIntegral n / 10000 

-- | Generar pseudoaleatorio de 4 decimales entre 0 y 1 (Congruencial lineal)
pseudoaleatorioCL :: Int -> Int -> Double
pseudoaleatorioCL x_n m = fromIntegral x_n / fromIntegral (m - 1)

constanteMultiplicador :: Int -> Int -> Int
constanteMultiplicador 3 = \k -> 3 + 8*k
constanteMultiplicador 5 = \k -> 5 + 8*k
constanteMultiplicador _ = error "Debe ser 3 o 5"
 
-- | Algoritmo congruencial lineal multiplicativo
congLinealMult :: Int -> Int -> Int -> Int -> Int 
congLinealMult x0 k g opt =
  let m = 2^g 
      a = constanteMultiplicador opt k  
  in (a*x0) `mod` m  
