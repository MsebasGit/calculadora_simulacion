module Funciones.Estadisticas where
import qualified Data.Vector.Unboxed as U
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (standard)
import Statistics.Distribution.ChiSquared (chiSquared)

pruebaDeMedias :: Double -> U.Vector Double -> (Double, Double, Bool)  
pruebaDeMedias alpha numeros = 
    let n = fromIntegral (U.length numeros)
        mediaMuestral = U.sum numeros / n
        zAlpha2 = quantile standard (1 - alpha / 2)
        errorEstandar = 1 / sqrt (12 * n)
        zCalculado = abs (mediaMuestral - 0.5) / errorEstandar
        pasaPrueba = zCalculado <= zAlpha2
    in (zCalculado, zAlpha2, pasaPrueba)  


-- | Función para realizar la prueba de varianza en U(0,1)
pruebaDeVarianza :: Double -> U.Vector Double -> (Double, Double, Bool)  
pruebaDeVarianza alpha numeros = 
    let nInt = U.length numeros
        n = fromIntegral nInt
        glInt = nInt - 1          -- Grados de libertad como entero (para ChiSquared)
        gl = fromIntegral glInt   -- Grados de libertad como Double (para matemáticas)
        
        -- Cálculo de la media
        mediaMuestral = U.sum numeros / n
        
        -- Cálculo de la varianza muestral (S^2)
        sumaDiferencias = U.sum $ U.map (\x -> (x - mediaMuestral)^(2::Int)) numeros
        varianzaMuestral = sumaDiferencias / gl
        
        -- Distribución Chi-cuadrada con n-1 grados de libertad
        distChi = chiSquared glInt
        
        -- Buscamos los cuantiles en la distribución
        chiInferior = quantile distChi (alpha / 2)
        chiSuperior = quantile distChi (1 - alpha / 2)
        
        chiCalculado = 12 * gl * varianzaMuestral
        pasaPrueba = chiCalculado >= chiInferior && chiCalculado <= chiSuperior
    in (chiCalculado, chiSuperior, pasaPrueba) 


mCalculado :: Double -> Int
mCalculado n = round (sqrt n)

pruebaChiCuadrada :: Double -> Int -> U.Vector Double -> (Double, Double, Bool) 
pruebaChiCuadrada alpha m k_numeros = 
    let n = fromIntegral (U.length k_numeros)
        m' = fromIntegral m
        -- 2. Frecuencia Esperada: E_i = n / m
        frecuenciaEsperada = n / m'
        
        -- Inicializamos un vector de tamaño 'm' lleno de ceros para contar las frecuencias (O_i)
        frecuenciasObservadas = U.accum (+) (U.replicate m 0.0) actualizaciones
          where
            actualizaciones = map (\x -> (obtenerIndice x m, 1.0)) (U.toList k_numeros)
            
            obtenerIndice :: Double -> Int -> Int
            obtenerIndice val bins = 
                let idx = floor (val * fromIntegral bins)
                in min (bins - 1) (max 0 idx) -- Asegura que no salga del rango [0, m-1]

        chiCalculado = U.sum $ U.map (\o_i -> ((o_i - frecuenciaEsperada) ** 2) / frecuenciaEsperada) frecuenciasObservadas
        glInt = m - 1
        distChi = chiSquared glInt
        chiCritico = quantile distChi (1 - alpha)
        pasaPrueba = chiCalculado <= chiCritico
    in (chiCalculado, chiCritico, pasaPrueba)  
