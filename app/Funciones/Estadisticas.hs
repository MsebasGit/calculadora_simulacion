module Funciones.Estadisticas where

import qualified Data.Vector.Unboxed as U
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (standard)
import Statistics.Distribution.ChiSquared (chiSquared)
import Statistics.Distribution.Uniform (uniformDistr)
import Statistics.Test.KolmogorovSmirnov (kolmogorovSmirnovTest)
import Statistics.Test.Types (Test(..))
import Statistics.Types (pValue)

-- | 1. PRUEBA DE MEDIAS (Uniformidad)
-- Retorna: (Z_calculado, Z_critico, PasaPrueba)
pruebaDeMedias :: Double -> U.Vector Double -> (Double, Double, Bool)  
pruebaDeMedias alpha numeros = 
    let n = fromIntegral (U.length numeros)
        mediaMuestral = U.sum numeros / n
        zAlpha2 = quantile standard (1 - alpha / 2)
        errorEstandar = 1 / sqrt (12 * n)
        zCalculado = (mediaMuestral - 0.5) / errorEstandar
        pasaPrueba = abs zCalculado <= zAlpha2
    in (zCalculado, zAlpha2, pasaPrueba)  

-- | 2. PRUEBA DE VARIANZA (Uniformidad)
-- Retorna: (Chi2_calculado, Chi2_superior, PasaPrueba)
pruebaDeVarianza :: Double -> U.Vector Double -> (Double, Double, Bool)  
pruebaDeVarianza alpha numeros = 
    let nInt = U.length numeros
        glInt = nInt - 1          
        gl = fromIntegral glInt   
        
        mediaMuestral = U.sum numeros / fromIntegral nInt
        sumaDiferencias = U.sum $ U.map (\x -> (x - mediaMuestral)^(2::Int)) numeros
        varianzaMuestral = sumaDiferencias / gl
        
        distChi = chiSquared glInt
        chiInferior = quantile distChi (alpha / 2)
        chiSuperior = quantile distChi (1 - alpha / 2)
        
        chiCalculado = (gl * varianzaMuestral) / (1 / 12)
        pasaPrueba = chiCalculado >= chiInferior && chiCalculado <= chiSuperior
    in (chiCalculado, chiSuperior, pasaPrueba) 

-- | Función auxiliar para calcular dinámicamente los intervalos recomendados (Raíz de N)
mCalculado :: Double -> Int
mCalculado n = round (sqrt n)

-- | 3. PRUEBA CHI-CUADRADA POR INTERVALOS (Uniformidad)
-- Retorna: (Chi2_calculado, Chi2_critico, PasaPrueba)
pruebaChiCuadrada :: Double -> Int -> U.Vector Double -> (Double, Double, Bool) 
pruebaChiCuadrada alpha m k_numeros = 
    let n = fromIntegral (U.length k_numeros)
        m' = fromIntegral m
        frecuenciaEsperada = n / m'
        
        frecuenciasObservadas = U.accum (+) (U.replicate m 0.0) actualizaciones
          where
            actualizaciones = map (\x -> (obtenerIndice x m, 1.0)) (U.toList k_numeros)
            
            obtenerIndice :: Double -> Int -> Int
            obtenerIndice val bins = 
                let idx = floor (val * fromIntegral bins)
                in min (bins - 1) (max 0 idx)

        chiCalculado = U.sum $ U.map (\o_i -> ((o_i - frecuenciaEsperada) ** 2) / frecuenciaEsperada) frecuenciasObservadas
        glInt = m - 1
        distChi = chiSquared glInt
        chiCritico = quantile distChi (1 - alpha)
        pasaPrueba = chiCalculado <= chiCritico
    in (chiCalculado, chiCritico, pasaPrueba)  

-- | 4. PRUEBA DE KOLMOGOROV-SMIRNOV (Uniformidad continua)
-- Retorna: (D_estadistico, P-Valor, PasaPrueba)
pruebaKolmogorovSmirnov :: Double -> U.Vector Double -> (Double, Double, Bool)
pruebaKolmogorovSmirnov alpha numeros =
    case kolmogorovSmirnovTest (uniformDistr 0 1) numeros of
        Nothing -> (0.0, alpha, False)
        Just t   ->
            let pVal = pValue (testSignificance t)
                pasaPrueba = pVal >= alpha
            in (pVal, alpha, pasaPrueba)  

-- | 5. PRUEBA DE CORRIDAS ARRIBA Y ABAJO (Independencia)
-- Retorna: (Z_calculado, Z_critico, PasaPrueba)
pruebaDeCorridas :: Double -> U.Vector Double -> (Double, Double, Bool)
pruebaDeCorridas alpha numeros =
    let n = fromIntegral (U.length numeros) :: Double
        
        -- Paso 1: Construcción de la Secuencia S de unos y ceros (r_{i+1} > r_i)
        pares = U.zip numeros (U.tail numeros)
        secuenciaS = U.map (\(x, y) -> if y > x then 1 else 0 :: Int) pares
        
        -- Paso 2: Contar los cambios de signo para hallar el total de corridas (C_0)
        cambios = U.filter (\(s1, s2) -> s1 /= s2) (U.zip secuenciaS (U.tail secuenciaS))
        c_0 = fromIntegral (1 + U.length cambios) :: Double
        
        -- Paso 3: Media y Varianza esperadas bajo la hipótesis de independencia
        mu_a = (2 * n - 1) / 3
        sigma2_a = (16 * n - 29) / 90
        
        -- Paso 4 y 5: Estadístico Z y valor crítico normal estándar
        zCalculado = (c_0 - mu_a) / sqrt sigma2_a
        zAlpha2 = quantile standard (1 - alpha / 2)
        
        -- Regla de decisión de dos colas
        pasaPrueba = abs zCalculado <= zAlpha2
    in (zCalculado, zAlpha2, pasaPrueba)
