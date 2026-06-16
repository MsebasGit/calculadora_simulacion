module Funciones.Congruenciales where

congruencialLineal :: Int -> Int -> Int -> Int -> Int
congruencialLineal x0 a c m = (a * x0 + c) `mod` m   

