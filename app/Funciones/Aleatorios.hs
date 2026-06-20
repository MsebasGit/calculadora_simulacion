module Funciones.Aleatorios
  ( congruencialLineal
  , centro
  , cuadradosMedios
  , productosMedios
  , multiplicadorConstante
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

