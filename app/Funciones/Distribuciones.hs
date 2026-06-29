module Funciones.Distribuciones where

import Statistics.Distribution (quantile)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Normal (normalDistr)

-- 1. Distribución Uniforme
uniforme :: Double -> Double -> Double -> Double
uniforme a b ri = a + (b - a) * ri

-- 2. Distribución K-Erlang
kErlang :: Double -> Double -> Double -> Double
kErlang k lambda ri = quantile (gammaDistr k (1 / lambda)) ri

-- 3. Distribución Exponencial
exponencial :: Double -> Double -> Double
exponencial lambda ri = - (log (1 - ri)) / lambda

-- 4. Distribución Gamma (Parametrización α y β)
gammaInversaAlpha :: Double -> Double -> Double -> Double
gammaInversaAlpha alpha beta ri = quantile (gammaDistr alpha beta) ri

-- 5. Distribución Normal
normalInversa :: Double -> Double -> Double -> Double
normalInversa mu sigma ri = quantile (normalDistr mu sigma) ri

-- 6. Distribución Weibull (3 Parámetros)
weibullInversa :: Double -> Double -> Double -> Double -> Double
weibullInversa gamma alpha beta ri = gamma + beta * ((- log (1 - ri)) ** (1 / alpha))
