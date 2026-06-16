module Funciones.NoCongruenciales where

centro :: Int -> Int 
centro n =
  let str = show n 
      strRelleno  = replicate (8 - length str) '0' ++ str
    
      strCentro   = take 4 (drop 2 strRelleno)
  in  read strCentro 

cuadradosMedios :: Int -> Int 
cuadradosMedios x0 = centro $ x0 ^ 2

{-
productosMedios :: Int -> Int -> Int
productosMedios x_cero x_uno =
  let y_n       = x_cero * x_uno
      x_mas_uno = centro y_n 
  in  x_mas_uno

pasoProductosMedios :: (Int, Int) -> (Int, Int)
pasoProductosMedios (x0, x1) = 
  let x2 = productosMedios x0 x1
  in (x1, x2) -- El viejo x1 ahora es el primero, y el nuevo x2 es el segundo
-}

productosMedios :: (Int, Int) -> (Int, Int)
productosMedios (x0, x1) = (x1, centro (x0 * x1))

multiplicadorConstante :: Int -> Int -> Int
multiplicadorConstante c x0 = centro $ x0 * c 
