{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SubAutomatas.AnalizadorEstadistico where

import Miso
import Miso.Html
import Miso.Lens
import Miso.Html.Property (class_)
import qualified Miso.Mathml as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.List (isInfixOf)
import Funciones.Estadisticas 

-- Importamos nuestro sub-autómata técnico
import SubAutomatas.InputValidado 


-- ==========================================
-- 1. TIPOS DE DATOS (El Modelo de Dominio)
-- ==========================================

data ResultadoPrueba = Resultado 
  { _estadisticoCalculado :: Double
  , _valorTeorico         :: Double
  , _pasaPrueba           :: Bool
  } deriving (Show, Eq)

data ResultadosMultiples = ResultadosMultiples
  { _resMedias      :: Maybe ResultadoPrueba
  , _resVarianza    :: Maybe ResultadoPrueba
  , _resChiCuadrada :: Maybe ResultadoPrueba
  , _resKolmogorov  :: Maybe ResultadoPrueba
  , _resCorridas    :: Maybe ResultadoPrueba
  } deriving (Show, Eq)

-- El Estado principal del analizador
data AnalizadorModel = AnalizadorModel
  { _nivelConfianza  :: Double
  , _inputConfianza  :: InputValidado
  
  , _intervalosChi   :: Maybe Int     -- Nothing = usar raíz de N
  , _inputIntervalos :: InputValidado -- Para que el usuario escriba
  
  , _resultados      :: ResultadosMultiples
  } deriving (Show, Eq)

-- ==========================================
-- 2. LENTES MANUALES
-- ==========================================

inputConfianza :: Lens AnalizadorModel InputValidado
inputConfianza = lens _inputConfianza $ \m x -> m { _inputConfianza = x }

inputIntervalos :: Lens AnalizadorModel InputValidado
inputIntervalos = lens _inputIntervalos $ \m x -> m { _inputIntervalos = x }

-- ==========================================
-- 3. ESTADO INICIAL (q0)
-- ==========================================

analizadorInicial :: AnalizadorModel
analizadorInicial = AnalizadorModel
  { _nivelConfianza  = 0.05
  , _inputConfianza  = InputValidado "0.05" Nothing
  
  , _intervalosChi   = Nothing
  , _inputIntervalos = InputValidado "" Nothing -- Vacío por defecto
  
  , _resultados      = ResultadosMultiples Nothing Nothing Nothing Nothing Nothing
  }

-- ==========================================
-- 4. ALFABETO (Acciones)
-- ==========================================

data AnalizadorAction
  = AccionConfianza InputValidadoAction
  | FijarConfianza
  
  | AccionIntervalos InputValidadoAction
  | FijarIntervalos
  
  | EjecutarPruebas (V.Vector Double)
  deriving (Show, Eq)

-- ==========================================
-- 5. TRANSICIÓN (Update)
-- ==========================================

updateAnalizador :: AnalizadorAction -> AnalizadorModel -> AnalizadorModel
updateAnalizador = \case

  AccionConfianza msg -> 
    inputConfianza %~ updateInputValidado msg

  FijarConfianza -> \modelo ->
    let texto = fromMisoString (_textoTemporal (_inputConfianza modelo))
    in case readMaybe texto of
         Just val -> modelo { _nivelConfianza = val, _inputConfianza = setTextoSinError (ms texto) }
         Nothing  -> inputConfianza %~ updateInputValidado (MostrarError "Debe ser un número (ej. 0.05)") $ modelo

  AccionIntervalos msg -> 
    inputIntervalos %~ updateInputValidado msg

  FijarIntervalos -> \modelo ->
    let texto = fromMisoString (_textoTemporal (_inputIntervalos modelo))
    in if null texto 
       then modelo { _intervalosChi = Nothing, _inputIntervalos = setTextoSinError "" } -- Vuelve al defecto
       else case readMaybe texto :: Maybe Int of
              Just val -> modelo { _intervalosChi = Just val, _inputIntervalos = setTextoSinError (ms texto) }
              Nothing  -> inputIntervalos %~ updateInputValidado (MostrarError "Debe ser entero") $ modelo

  EjecutarPruebas datos -> \modelo ->
    let 
        n = V.length datos
        alpha = _nivelConfianza modelo
        
        -- Si el usuario no dio intervalos (Nothing), calculamos la raíz de N
        k = case _intervalosChi modelo of
              Just valorDefinido -> valorDefinido
              Nothing            -> floor (sqrt (fromIntegral n :: Double))
        
        datosUnboxed = U.convert datos

        (calcM, critM, pasaM) = pruebaDeMedias alpha datosUnboxed
        resM = Resultado calcM critM pasaM

        (calcV, critV, pasaV) = pruebaDeVarianza alpha datosUnboxed
        resV = Resultado calcV critV pasaV

        (calcC, critC, pasaC) = pruebaChiCuadrada alpha k datosUnboxed
        resC = Resultado calcC critC pasaC
        
        (calcK, critK, pasaK) = pruebaKolmogorovSmirnov alpha datosUnboxed
        resK = Resultado calcK critK pasaK
        
        (calcR, critR, pasaR) = pruebaDeCorridas alpha datosUnboxed
        resR = Resultado calcR critR pasaR
        
        nuevosResultados = ResultadosMultiples 
          { _resMedias      = Just resM
          , _resVarianza    = Just resV
          , _resChiCuadrada = Just resC
          , _resKolmogorov  = Just resK
          , _resCorridas    = Just resR
          }
    in modelo { _resultados = nuevosResultados }

-- ==========================================
-- 6. LA VISTA (View)
-- ==========================================

-- Fórmulas MathML para cada prueba
formulaMedias :: View model action
formulaMedias = M.math_ []
  [ M.msub_ [] [ M.mi_ [] [text "Z"], M.mn_ [] [text "0"] ]
  , M.mo_ [] [text "="]
  , M.mfrac_ []
      [ M.mrow_ []
          [ M.mo_ [] [text "|"], M.mover_ [] [ M.mi_ [] [text "x"], M.mo_ [] [text "¯"] ], M.mo_ [] [text "−"], M.mn_ [] [text "0.5"], M.mo_ [] [text "|"] ]
      , M.mfrac_ []
          [ M.mn_ [] [text "1"]
          , M.msqrt_ [] [ M.mrow_ [] [ M.mn_ [] [text "12"], M.mi_ [] [text "n"] ] ]
          ]
      ]
  ]

formulaVarianza :: View model action
formulaVarianza = M.math_ []
  [ M.msubsup_ [] [ M.mi_ [] [text "χ"], M.mn_ [] [text "0"], M.mn_ [] [text "2"] ]
  , M.mo_ [] [text "="]
  , M.mrow_ []
      [ M.mn_ [] [text "12"], M.mo_ [] [text "·"], M.mo_ [] [text "("], M.mi_ [] [text "n"], M.mo_ [] [text "−"], M.mn_ [] [text "1"], M.mo_ [] [text ")"], M.mo_ [] [text "·"], M.msup_ [] [ M.mi_ [] [text "S"], M.mn_ [] [text "2"] ] ]
  ]

formulaChiCuadrada :: View model action
formulaChiCuadrada = M.math_ []
  [ M.msubsup_ [] [ M.mi_ [] [text "χ"], M.mn_ [] [text "0"], M.mn_ [] [text "2"] ]
  , M.mo_ [] [text "="]
  , M.msubsup_ [] [ M.mo_ [] [text "∑"], M.mrow_ [] [ M.mi_ [] [text "i"], M.mo_ [] [text "="], M.mn_ [] [text "1"] ], M.mi_ [] [text "k"] ]
  , M.mfrac_ []
      [ M.msup_ []
          [ M.mrow_ [] [ M.mo_ [] [text "("], M.msub_ [] [ M.mi_ [] [text "O"], M.mi_ [] [text "i"] ], M.mo_ [] [text "−"], M.msub_ [] [ M.mi_ [] [text "E"], M.mi_ [] [text "i"] ], M.mo_ [] [text ")"] ]
          , M.mn_ [] [text "2"]
          ]
      , M.msub_ [] [ M.mi_ [] [text "E"], M.mi_ [] [text "i"] ]
      ]
  ]

formulaKolmogorovSmirnov :: View model action
formulaKolmogorovSmirnov = M.math_ []
  [ M.mi_ [] [text "D"]
  , M.mo_ [] [text "="]
  , M.mrow_ []
      [ M.mi_ [] [text "max"], M.mo_ [] [text "|"], M.msub_ [] [ M.mi_ [] [text "F"], M.mi_ [] [text "n"] ], M.mo_ [] [text "("], M.mi_ [] [text "x"], M.mo_ [] [text ")"], M.mo_ [] [text "−"], M.mi_ [] [text "F"], M.mo_ [] [text "("], M.mi_ [] [text "x"], M.mo_ [] [text ")"], M.mo_ [] [text "|"] ]
  ]

formulaCorridas :: View model action
formulaCorridas = M.math_ []
  [ M.mi_ [] [text "Z"]
  , M.mo_ [] [text "="]
  , M.mfrac_ []
      [ M.mrow_ []
          [ M.msub_ [] [ M.mi_ [] [text "C"], M.mn_ [] [text "0"] ], M.mo_ [] [text "−"], M.msub_ [] [ M.mi_ [] [text "μ"], M.mi_ [] [text "C"] ] ]
      , M.msub_ [] [ M.mi_ [] [text "σ"], M.mi_ [] [text "C"] ]
      ]
  ]

viewAnalizador :: AnalizadorModel -> View model AnalizadorAction
viewAnalizador modelo = div_ [  ]
  [ h2_ [] [ text "Análisis Estadístico" ]
  
  , div_ [  ]
      [ -- Control de Confianza
        div_ [] 
          [ label_ [] [ text "Nivel de Confianza (Alpha):" ]
          , viewInputValidado AccionConfianza (_inputConfianza modelo)
          , button_ [ onClick FijarConfianza ] [ text "Guardar Alpha" ]
          ]
          
      , -- Control de Intervalos
        div_ []
          [ label_ [] [ text "Intervalos Chi-Cuadrada (Vacío = √n):" ]
          , viewInputValidado AccionIntervalos (_inputIntervalos modelo)
          , button_ [ onClick FijarIntervalos ] [ text "Guardar K" ]
          ]
      ]

  -- Resultados en cuadrícula premium
  , hr_ []
  , h3_ [] [ text "Resultados" ]
  , div_ [ class_ "results-container" ]
      [ -- Sección de Uniformidad
        div_ [ class_ "results-section" ]
          [ h4_ [] [ text "Pruebas de Uniformidad" ]
          , div_ [ class_ "results-grid" ]
              [ vistaResultado 
                  "Prueba de Medias" 
                  "Compara la media muestral obtenida con el valor esperado de 0.5 bajo uniformidad." 
                  formulaMedias 
                  (_resMedias (_resultados modelo))
              , vistaResultado 
                  "Prueba de Varianza" 
                  "Compara la varianza muestral frente al valor esperado teórico de 1/12 (~0.0833)." 
                  formulaVarianza 
                  (_resVarianza (_resultados modelo))
              , vistaResultado 
                  "Prueba Chi-Cuadrada" 
                  "Compara las frecuencias observadas en sub-intervalos con las frecuencias uniformes esperadas." 
                  formulaChiCuadrada 
                  (_resChiCuadrada (_resultados modelo))
              , vistaResultado 
                  "Prueba Kolmogorov-Smirnov" 
                  "Evalúa la máxima desviación absoluta entre la distribución empírica y la teórica continua." 
                  formulaKolmogorovSmirnov 
                  (_resKolmogorov (_resultados modelo))
              ]
          ]
      , hr_ []
      -- Sección de Independencia
      , div_ [ class_ "results-section" ]
          [ h4_ [] [ text "Pruebas de Independencia" ]
          , div_ [ class_ "results-grid" ]
              [ vistaResultado 
                  "Prueba de Corridas" 
                  "Cuenta la cantidad de rachas de crecimiento/decrecimiento para evaluar la independencia secuencial." 
                  formulaCorridas 
                  (_resCorridas (_resultados modelo))
              ]
          ]
      ]
  ]

-- Función auxiliar (UI Pura) para dibujar las tarjetas de resultados
vistaResultado :: MisoString -> MisoString -> View model action -> Maybe ResultadoPrueba -> View model action
vistaResultado titulo significado formula Nothing = 
  div_ [ class_ "result-card" ]
    [ div_ [ class_ "result-header" ]
        [ h5_ [ class_ "result-title" ] [ text titulo ] ]
    , p_ [ class_ "result-description" ] [ text significado ]
    , div_ [ class_ "result-formula" ] [ formula ]
    , div_ [ class_ "result-stats" ] [ text "Esperando datos..." ]
    ]
vistaResultado titulo significado formula (Just res) = 
  let pasa       = _pasaPrueba res
      mensaje :: String
      mensaje    = if pasa then "Aprobado" else "Reprobado"
      badgeClass :: String
      badgeClass = if pasa then "result-badge badge-pass" else "result-badge badge-fail"
      esKS       = "Kolmogorov" `isInfixOf` fromMisoString titulo
      lblCritico = if esKS then "P-Valor: " else "Valor Crítico: "
      
      calcStr = printf "%.4f" (_estadisticoCalculado res) :: String
      teorStr = printf "%.4f" (_valorTeorico res) :: String
      lblCriticoStr :: String
      lblCriticoStr = lblCritico
  in div_ [ class_ "result-card" ]
       [ div_ [ class_ "result-header" ]
           [ h5_ [ class_ "result-title" ] [ text titulo ]
           , span_ [ class_ (ms badgeClass) ] [ text (ms mensaje) ]
           ]
       , p_ [ class_ "result-description" ] [ text significado ]
       , div_ [ class_ "result-formula" ] [ formula ]
       , div_ [ class_ "result-stats" ]
           [ div_ [] [ text ("Estadístico: " <> ms calcStr) ]
           , div_ [] [ text (ms lblCriticoStr <> ms teorStr) ]
           ]
       ]

-- Helper para limpiar errores al guardar exitosamente
setTextoSinError :: MisoString -> InputValidado
setTextoSinError t = InputValidado t Nothing
