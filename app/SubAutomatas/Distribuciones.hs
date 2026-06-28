{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module SubAutomatas.Distribuciones
  ( DistribucionTipo (..)
  , DistribucionModel (..)
  , DistribucionAction (..)
  , distribucionesInicial
  , updateDistribuciones
  , viewDistribuciones
  , establecerDatosRi
  ) where

import Miso hiding ((!!))
import qualified Miso.Html as H
import qualified Miso.Svg as S
import Miso.Html.Property (value_, type_, class_, selected_)
import Miso.Html.Event (onChange, onClick)
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Text.Printf (printf)
import Control.Lens

import SubAutomatas.InputValidado
import qualified Funciones.Distribuciones as FD

-- | Tipos de distribuciones soportadas
data DistribucionTipo
  = DistUniforme
  | DistExponencial
  | DistKErlang
  | DistGammaAlpha
  | DistNormal
  | DistWeibull
  deriving (Show, Eq, Enum, Bounded)

-- | Estructura para almacenar un resultado calculado
data ResultadoCalc = ResultadoCalc
  { _rcTipo    :: DistribucionTipo
  , _rcParams  :: [Double]
  , _rcValores :: V.Vector Double
  } deriving (Show, Eq)

-- | Modelo de datos del sub-automáta de distribuciones
data DistribucionModel = DistribucionModel
  { _tipoSeleccionado     :: DistribucionTipo
  , _paramA               :: InputValidado
  , _paramB               :: InputValidado
  , _paramLambda          :: InputValidado
  , _paramK               :: InputValidado
  , _paramAlpha           :: InputValidado
  , _paramBeta            :: InputValidado
  , _paramMu              :: InputValidado
  , _paramSigma           :: InputValidado
  , _paramGamma           :: InputValidado
  , _datosRi              :: V.Vector Double
  , _resultadosCalculados :: [ResultadoCalc]
  } deriving (Show, Eq)

-- | Acciones locales
data DistribucionAction
  = SeleccionarTipo DistribucionTipo
  | ActionParamA InputValidadoAction
  | ActionParamB InputValidadoAction
  | ActionParamLambda InputValidadoAction
  | ActionParamK InputValidadoAction
  | ActionParamAlpha InputValidadoAction
  | ActionParamBeta InputValidadoAction
  | ActionParamMu InputValidadoAction
  | ActionParamSigma InputValidadoAction
  | ActionParamGamma InputValidadoAction
  | Calcular
  | EliminarResultado Int
  | EstablecerRi (V.Vector Double)
  deriving (Show, Eq)

-- | Estado inicial
distribucionesInicial :: DistribucionModel
distribucionesInicial = DistribucionModel
  { _tipoSeleccionado     = DistUniforme
  , _paramA               = InputValidado "0.0" Nothing
  , _paramB               = InputValidado "1.0" Nothing
  , _paramLambda          = InputValidado "1.0" Nothing
  , _paramK               = InputValidado "2.0" Nothing
  , _paramAlpha           = InputValidado "2.0" Nothing
  , _paramBeta            = InputValidado "1.0" Nothing
  , _paramMu              = InputValidado "0.0" Nothing
  , _paramSigma           = InputValidado "1.0" Nothing
  , _paramGamma           = InputValidado "0.0" Nothing
  , _datosRi              = V.empty
  , _resultadosCalculados = []
  }

-- | Helper para leer Double
readMaybeDouble :: InputValidado -> Maybe Double
readMaybeDouble iv = readMaybe (fromMisoString (_textoTemporal iv))

-- | Helper para setear error
setError :: MisoString -> InputValidado -> InputValidado
setError msg iv = iv { _errorActual = Just msg }

-- | Helper para limpiar error
clearError :: InputValidado -> InputValidado
clearError iv = iv { _errorActual = Nothing }

-- | Establecer datos Ri desde fuera
establecerDatosRi :: V.Vector Double -> DistribucionModel -> DistribucionModel
establecerDatosRi datos model = model { _datosRi = datos, _resultadosCalculados = [] }

-- | Realiza las validaciones y el cálculo matemático
validarYCalcular :: DistribucionModel -> DistribucionModel
validarYCalcular model =
  let ri = _datosRi model
  in if V.null ri
     then model
     else case _tipoSeleccionado model of
       DistUniforme ->
         case (readMaybeDouble (_paramA model), readMaybeDouble (_paramB model)) of
           (Just a, Just b)
             | a < b ->
                 let xi = V.map (FD.uniforme a b) ri
                     res = ResultadoCalc DistUniforme [a, b] xi
                 in model { _resultadosCalculados = res : _resultadosCalculados model }
             | otherwise ->
                 model { _paramB = setError "B debe ser mayor que A" (_paramB model) }
           (Nothing, _) -> model { _paramA = setError "Valor inválido" (_paramA model) }
           (_, Nothing) -> model { _paramB = setError "Valor inválido" (_paramB model) }
           
       DistExponencial ->
         case readMaybeDouble (_paramLambda model) of
           Just lambda
             | lambda > 0 ->
                 let xi = V.map (FD.exponencial lambda) ri
                     res = ResultadoCalc DistExponencial [lambda] xi
                 in model { _resultadosCalculados = res : _resultadosCalculados model }
             | otherwise ->
                 model { _paramLambda = setError "La tasa debe ser mayor a 0" (_paramLambda model) }
           Nothing -> model { _paramLambda = setError "Valor inválido" (_paramLambda model) }

       DistKErlang ->
         case (readMaybeDouble (_paramK model), readMaybeDouble (_paramLambda model)) of
           (Just kVal, Just lambda)
             | kVal > 0 && lambda > 0 ->
                 let xi = V.map (FD.kErlang kVal lambda) ri
                     res = ResultadoCalc DistKErlang [kVal, lambda] xi
                 in model { _resultadosCalculados = res : _resultadosCalculados model }
             | otherwise ->
                 let mK = if kVal <= 0 then setError "Forma k debe ser > 0" (_paramK model) else _paramK model
                     mL = if lambda <= 0 then setError "Tasa λ debe ser > 0" (_paramLambda model) else _paramLambda model
                 in model { _paramK = mK, _paramLambda = mL }
           (Nothing, _) -> model { _paramK = setError "Valor inválido" (_paramK model) }
           (_, Nothing) -> model { _paramLambda = setError "Valor inválido" (_paramLambda model) }

       DistGammaAlpha ->
         case (readMaybeDouble (_paramAlpha model), readMaybeDouble (_paramBeta model)) of
           (Just alphaVal, Just betaVal)
             | alphaVal > 0 && betaVal > 0 ->
                 let xi = V.map (FD.gammaInversaAlpha alphaVal betaVal) ri
                     res = ResultadoCalc DistGammaAlpha [alphaVal, betaVal] xi
                 in model { _resultadosCalculados = res : _resultadosCalculados model }
             | otherwise ->
                 let mA = if alphaVal <= 0 then setError "Forma α debe ser > 0" (_paramAlpha model) else _paramAlpha model
                     mB = if betaVal <= 0 then setError "Escala β debe ser > 0" (_paramBeta model) else _paramBeta model
                 in model { _paramAlpha = mA, _paramBeta = mB }
           (Nothing, _) -> model { _paramAlpha = setError "Valor inválido" (_paramAlpha model) }
           (_, Nothing) -> model { _paramBeta = setError "Valor inválido" (_paramBeta model) }

       DistNormal ->
         case (readMaybeDouble (_paramMu model), readMaybeDouble (_paramSigma model)) of
           (Just muVal, Just sigmaVal)
             | sigmaVal > 0 ->
                 let xi = V.map (FD.normalInversa muVal sigmaVal) ri
                     res = ResultadoCalc DistNormal [muVal, sigmaVal] xi
                 in model { _resultadosCalculados = res : _resultadosCalculados model }
             | otherwise ->
                 model { _paramSigma = setError "Desviación estándar debe ser > 0" (_paramSigma model) }
           (Nothing, _) -> model { _paramMu = setError "Valor inválido" (_paramMu model) }
           (_, Nothing) -> model { _paramSigma = setError "Valor inválido" (_paramSigma model) }

       DistWeibull ->
         case (readMaybeDouble (_paramGamma model), readMaybeDouble (_paramAlpha model), readMaybeDouble (_paramBeta model)) of
           (Just gammaVal, Just alphaVal, Just betaVal)
             | alphaVal > 0 && betaVal > 0 ->
                 let xi = V.map (FD.weibullInversa gammaVal alphaVal betaVal) ri
                     res = ResultadoCalc DistWeibull [gammaVal, alphaVal, betaVal] xi
                 in model { _resultadosCalculados = res : _resultadosCalculados model }
             | otherwise ->
                 let mA = if alphaVal <= 0 then setError "Forma α debe ser > 0" (_paramAlpha model) else _paramAlpha model
                     mB = if betaVal <= 0 then setError "Escala β debe ser > 0" (_paramBeta model) else _paramBeta model
                 in model { _paramAlpha = mA, _paramBeta = mB }
           (Nothing, _, _) -> model { _paramGamma = setError "Valor inválido" (_paramGamma model) }
           (_, Nothing, _) -> model { _paramAlpha = setError "Valor inválido" (_paramAlpha model) }
           (_, _, Nothing) -> model { _paramBeta = setError "Valor inválido" (_paramBeta model) }

-- | Update de estado local
updateDistribuciones :: DistribucionAction -> DistribucionModel -> DistribucionModel
updateDistribuciones action model = case action of
  SeleccionarTipo t ->
    model { _tipoSeleccionado = t }
    
  ActionParamA sub -> model { _paramA = updateInputValidado sub (_paramA model) }
  ActionParamB sub -> model { _paramB = updateInputValidado sub (_paramB model) }
  ActionParamLambda sub -> model { _paramLambda = updateInputValidado sub (_paramLambda model) }
  ActionParamK sub -> model { _paramK = updateInputValidado sub (_paramK model) }
  ActionParamAlpha sub -> model { _paramAlpha = updateInputValidado sub (_paramAlpha model) }
  ActionParamBeta sub -> model { _paramBeta = updateInputValidado sub (_paramBeta model) }
  ActionParamMu sub -> model { _paramMu = updateInputValidado sub (_paramMu model) }
  ActionParamSigma sub -> model { _paramSigma = updateInputValidado sub (_paramSigma model) }
  ActionParamGamma sub -> model { _paramGamma = updateInputValidado sub (_paramGamma model) }

  Calcular ->
    let modelCleared = model
          { _paramA = clearError (_paramA model)
          , _paramB = clearError (_paramB model)
          , _paramLambda = clearError (_paramLambda model)
          , _paramK = clearError (_paramK model)
          , _paramAlpha = clearError (_paramAlpha model)
          , _paramBeta = clearError (_paramBeta model)
          , _paramMu = clearError (_paramMu model)
          , _paramSigma = clearError (_paramSigma model)
          , _paramGamma = clearError (_paramGamma model)
          }
    in validarYCalcular modelCleared

  EliminarResultado idx ->
    let res = _resultadosCalculados model
        nuevosRes = [ r | (i, r) <- zip [0..] res, i /= idx ]
    in model { _resultadosCalculados = nuevosRes }

  EstablecerRi datos ->
    model { _datosRi = datos, _resultadosCalculados = [] }

-- | Retorna el nombre legible
nombreDistribucion :: DistribucionTipo -> String
nombreDistribucion = \case
  DistUniforme    -> "Uniforme"
  DistExponencial -> "Exponencial"
  DistKErlang     -> "K-Erlang (k, λ)"
  DistGammaAlpha  -> "Gamma (α, β)"
  DistNormal      -> "Normal"
  DistWeibull     -> "Weibull"

-- | Calcula frecuencias para el histograma
calcularFrecuencias :: Int -> V.Vector Double -> (Double, Double, [Int], Double)
calcularFrecuencias k v =
  if V.null v
  then (0.0, 0.0, [], 0.0)
  else
    let values = V.toList v
        minVal = minimum values
        maxVal = maximum values
        range = maxVal - minVal
        (adjMin, adjMax, adjRange) =
          if range == 0
          then (minVal - 1, minVal + 1, 2.0)
          else (minVal, maxVal, range)
        binWidth = adjRange / fromIntegral k
        contadores = replicate k 0
        incrementarBin acc x =
          let idx = floor ((x - adjMin) / binWidth)
              clampedIdx = max 0 (min (k - 1) idx)
              (left, pivot : right) = splitAt clampedIdx acc
          in left ++ (pivot + 1) : right
        freqs = foldl incrementarBin contadores values
        maxFreq = fromIntegral (maximum (0 : freqs))
    in (adjMin, adjMax, freqs, maxFreq)

-- | Renderizador de Histogramas en SVG
renderHistograma :: MisoString -> MisoString -> V.Vector Double -> View model action
renderHistograma titulo color v =
  if V.null v
  then H.div_ [ class_ "histogram-empty" ] [ text "No hay datos suficientes" ]
  else
    let k = 10 
        (minVal, maxVal, freqs, maxFreq) = calcularFrecuencias k v
        
        svgWidth = 400 :: Double
        svgHeight = 180 :: Double
        marginTop = 15 :: Double
        marginBottom = 25 :: Double
        marginLeft = 35 :: Double
        marginRight = 15 :: Double
        
        chartWidth = svgWidth - marginLeft - marginRight
        chartHeight = svgHeight - marginTop - marginBottom
        
        binWidth = chartWidth / fromIntegral k
        barSpacing = 2 :: Double
        
        dibujarBarra :: Int -> Int -> View model action
        dibujarBarra idx freq =
          let f = fromIntegral freq
              h = if maxFreq == 0 then 0 else (f / maxFreq) * chartHeight
              x = marginLeft + fromIntegral idx * binWidth + barSpacing / 2
              y = svgHeight - marginBottom - h
              w = binWidth - barSpacing
              
              rMin = minVal + fromIntegral idx * ((maxVal - minVal) / fromIntegral k)
              rMax = minVal + fromIntegral (idx + 1) * ((maxVal - minVal) / fromIntegral k)
              tooltipText = printf "Rango: [%.3f, %.3f]\nFrecuencia: %d" rMin rMax freq :: String
          in S.g_ []
               [ S.title_ [] [ text (ms tooltipText) ]
               , S.rect_
                   [ textProp "x" (ms (show x))
                   , textProp "y" (ms (show y))
                   , textProp "width" (ms (show w))
                   , textProp "height" (ms (show h))
                   , textProp "fill" color
                   , textProp "rx" "2"
                   , class_ "histogram-bar"
                   ]
               , if freq > 0 && h > 15
                 then S.text_
                        [ textProp "x" (ms (show (x + w/2)))
                        , textProp "y" (ms (show (y - 4)))
                        , class_ "histogram-bar-text"
                        ]
                        [ text (ms (show freq)) ]
                 else text ""
               ]
               
        barras = zipWith dibujarBarra [0..] freqs
        
        ejeX = S.line_
          [ textProp "x1" (ms (show marginLeft))
          , textProp "y1" (ms (show (svgHeight - marginBottom)))
          , textProp "x2" (ms (show (svgWidth - marginRight)))
          , textProp "y2" (ms (show (svgHeight - marginBottom)))
          , class_ "histogram-axis"
          ]
        ejeY = S.line_
          [ textProp "x1" (ms (show marginLeft))
          , textProp "y1" (ms (show marginTop))
          , textProp "x2" (ms (show marginLeft))
          , textProp "y2" (ms (show (svgHeight - marginBottom)))
          , class_ "histogram-axis"
          ]
          
        etiquetaMin = S.text_
          [ textProp "x" (ms (show marginLeft))
          , textProp "y" (ms (show (svgHeight - 10)))
          , class_ "histogram-axis-text text-start"
          ]
          [ text (ms (printf "%.2f" minVal :: String)) ]
          
        etiquetaMax = S.text_
          [ textProp "x" (ms (show (svgWidth - marginRight)))
          , textProp "y" (ms (show (svgHeight - 10)))
          , class_ "histogram-axis-text text-end"
          ]
          [ text (ms (printf "%.2f" maxVal :: String)) ]

        etiquetaMed = S.text_
          [ textProp "x" (ms (show (marginLeft + chartWidth / 2)))
          , textProp "y" (ms (show (svgHeight - 10)))
          , class_ "histogram-axis-text text-center"
          ]
          [ text (ms (printf "%.2f" (minVal + (maxVal - minVal)/2) :: String)) ]
          
    in H.div_ [ class_ "histogram-chart-wrapper" ]
         [ H.h5_ [ class_ "histogram-chart-title" ] [ text titulo ]
         , H.div_ [ class_ "histogram-svg-container" ]
             [ S.svg_
                 [ textProp "width" "100%"
                 , textProp "height" "100%"
                 , textProp "viewBox" "0 0 400 180"
                 , class_ "histogram-svg-el"
                 ]
                 ( ejeX : ejeY : etiquetaMin : etiquetaMax : etiquetaMed : barras )
             ]
         ]

-- | Inputs según la distribución seleccionada
viewInputsParametros :: DistribucionModel -> View model DistribucionAction
viewInputsParametros model =
  case _tipoSeleccionado model of
    DistUniforme ->
      H.div_ [ class_ "dist-params-grid" ]
        [ H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Mínimo (a):" ]
            , viewInputValidado ActionParamA (_paramA model)
            ]
        , H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Máximo (b):" ]
            , viewInputValidado ActionParamB (_paramB model)
            ]
        ]
        
    DistExponencial ->
      H.div_ [ class_ "dist-params-grid" ]
        [ H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Tasa (λ > 0):" ]
            , viewInputValidado ActionParamLambda (_paramLambda model)
            ]
        ]
        
    DistKErlang ->
      H.div_ [ class_ "dist-params-grid" ]
        [ H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Forma (k > 0):" ]
            , viewInputValidado ActionParamK (_paramK model)
            ]
        , H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Tasa (λ > 0):" ]
            , viewInputValidado ActionParamLambda (_paramLambda model)
            ]
        ]
        
    DistGammaAlpha ->
      H.div_ [ class_ "dist-params-grid" ]
        [ H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Forma (α > 0):" ]
            , viewInputValidado ActionParamAlpha (_paramAlpha model)
            ]
        , H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Escala (β > 0):" ]
            , viewInputValidado ActionParamBeta (_paramBeta model)
            ]
        ]
        
    DistNormal ->
      H.div_ [ class_ "dist-params-grid" ]
        [ H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Media (μ):" ]
            , viewInputValidado ActionParamMu (_paramMu model)
            ]
        , H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Desv. Est. (σ > 0):" ]
            , viewInputValidado ActionParamSigma (_paramSigma model)
            ]
        ]
        
    DistWeibull ->
      H.div_ [ class_ "dist-params-grid" ]
        [ H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Localización (γ):" ]
            , viewInputValidado ActionParamGamma (_paramGamma model)
            ]
        , H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Forma (α > 0):" ]
            , viewInputValidado ActionParamAlpha (_paramAlpha model)
            ]
        , H.div_ [ class_ "param-control" ]
            [ H.label_ [] [ text "Escala (β > 0):" ]
            , viewInputValidado ActionParamBeta (_paramBeta model)
            ]
        ]

-- | Vista principal del sub-autómata
viewDistribuciones :: DistribucionModel -> View model DistribucionAction
viewDistribuciones model =
  H.div_ [ class_ "distribuciones-container card fade-in" ]
    [ H.h3_ [ class_ "distribuciones-header" ] [ text "Transformación a Distribuciones de Probabilidad" ]
    , H.p_ [ class_ "distribuciones-intro" ] 
        [ text "¡Pruebas superadas con éxito! Utiliza tus números pseudoaleatorios aprobados para generar variables aleatorias." ]
    
    , H.div_ [ class_ "dist-setup-grid" ]
        [ H.div_ [ class_ "selector-wrapper" ]
            [ H.label_ [] [ text "Distribución Objetivo:" ]
            , H.select_ 
                [ onChange ( \case
                    "Uniforme"    -> SeleccionarTipo DistUniforme
                    "Exponencial" -> SeleccionarTipo DistExponencial
                    "KErlang"     -> SeleccionarTipo DistKErlang
                    "GammaAlpha"  -> SeleccionarTipo DistGammaAlpha
                    "Normal"      -> SeleccionarTipo DistNormal
                    "Weibull"     -> SeleccionarTipo DistWeibull
                    _             -> SeleccionarTipo DistUniforme
                  )
                , class_ "input-field select-field"
                ]
                [ H.option_ [ value_ "Uniforme", selected_ (tipo == DistUniforme) ] [ text "Distribución Uniforme (a, b)" ]
                , H.option_ [ value_ "Exponencial", selected_ (tipo == DistExponencial) ] [ text "Distribución Exponencial (λ)" ]
                , H.option_ [ value_ "KErlang", selected_ (tipo == DistKErlang) ] [ text "Distribución K-Erlang (k, λ)" ]
                , H.option_ [ value_ "GammaAlpha", selected_ (tipo == DistGammaAlpha) ] [ text "Distribución Gamma (α, β)" ]
                , H.option_ [ value_ "Normal", selected_ (tipo == DistNormal) ] [ text "Distribución Normal (μ, σ)" ]
                , H.option_ [ value_ "Weibull", selected_ (tipo == DistWeibull) ] [ text "Distribución Weibull (γ, α, β)" ]
                ]
            ]
        , viewInputsParametros model
        , H.div_ [ class_ "calc-btn-container" ]
            [ H.button_ [ onClick Calcular, class_ "btn-primary btn-calc-dist" ] [ text "Calcular y Añadir" ] ]
        ]
        
    , H.hr_ []
    
    -- Panel de Histogramas
    , H.div_ [ class_ "charts-dashboard" ]
        [ H.div_ [ class_ "original-chart-box" ]
            [ renderHistograma "Histograma de Pseudoaleatorios (R_i)" "#4f46e5" (_datosRi model) ]
        , if null (_resultadosCalculados model)
          then H.div_ [ class_ "no-charts-box" ]
                 [ text "Genera una distribución para ver el histograma de las variables transformadas." ]
          else H.div_ [ class_ "calculated-charts-grid" ]
                 [ renderCardResultado idx rc
                 | (idx, rc) <- zip [0..] (_resultadosCalculados model)
                 ]
        ]
    ]
  where
    tipo = _tipoSeleccionado model
    
    -- selected_ is imported from Miso.Html.Property
    
    renderCardResultado :: Int -> ResultadoCalc -> View model DistribucionAction
    renderCardResultado idx rc =
      let tipoStr = nombreDistribucion (_rcTipo rc)
          paramsStr = case _rcTipo rc of
            DistUniforme    -> printf "a = %.2f, b = %.2f" (p 0) (p 1)
            DistExponencial -> printf "λ = %.2f" (p 0)
            DistKErlang     -> printf "k = %.2f, λ = %.2f" (p 0) (p 1)
            DistGammaAlpha  -> printf "α = %.2f, β = %.2f" (p 0) (p 1)
            DistNormal      -> printf "μ = %.2f, σ = %.2f" (p 0) (p 1)
            DistWeibull     -> printf "γ = %.2f, α = %.2f, β = %.2f" (p 0) (p 1) (p 2)
          p i = (_rcParams rc) !! i
          tituloCard = tipoStr ++ " (" ++ paramsStr ++ ")"
      in H.div_ [ class_ "dist-result-card fade-in" ]
           [ H.div_ [ class_ "dist-result-card-header" ]
               [ H.span_ [ class_ "dist-card-title" ] [ text (ms tituloCard) ]
               , H.button_ [ onClick (EliminarResultado idx), class_ "btn-danger-small" ] [ text "Eliminar" ]
               ]
           , H.div_ [ class_ "dist-result-card-body" ]
               [ renderHistograma (ms ("Frecuencia de X_i (" ++ tipoStr ++ ")")) "#10b981" (_rcValores rc)
               , viewMuestraValores (_datosRi model) (_rcValores rc)
               ]
           ]

    -- Primeras 5 conversiones
    viewMuestraValores :: V.Vector Double -> V.Vector Double -> View model action
    viewMuestraValores ris xis =
      let n = 5
          pares = zip (V.toList (V.take n ris)) (V.toList (V.take n xis))
          filas = [ H.tr_ []
                      [ H.td_ [ class_ "text-muted" ] [ text (ms (printf "R_%d = %.4f" (i+1) r :: String)) ]
                      , H.td_ [ class_ "text-arrow" ] [ text "→" ]
                      , H.td_ [ class_ "text-highlight" ] [ text (ms (printf "X_%d = %.4f" (i+1) x :: String)) ]
                      ]
                  | (i, (r, x)) <- zip [0..] pares :: [(Int, (Double, Double))]
                  ]
      in H.div_ [ class_ "muestra-valores-box" ]
           [ H.span_ [ class_ "muestra-valores-lbl" ] [ text "Muestra de transformaciones:" ]
           , H.table_ [ class_ "muestra-valores-tbl" ]
               [ H.tbody_ [] filas ]
           ]
