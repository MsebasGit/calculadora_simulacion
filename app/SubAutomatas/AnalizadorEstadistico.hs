{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SubAutomatas.AnalizadorEstadistico where

import Miso
import Miso.Html
import Miso.Lens
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Text.Read (readMaybe)
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
  
  , _resultados      = ResultadosMultiples Nothing Nothing Nothing Nothing
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
        
        nuevosResultados = ResultadosMultiples 
          { _resMedias      = Just resM
          , _resVarianza    = Just resV
          , _resChiCuadrada = Just resC
          , _resKolmogorov  = Just resK
          }
    in modelo { _resultados = nuevosResultados }

-- ==========================================
-- 6. LA VISTA (View)
-- ==========================================

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

  -- El botón de ejecución vendrá desde fuera (el GlobalUpdate inyectará la acción)
  -- Pero mostramos los resultados aquí
  , hr_ []
  , h3_ [] [ text "Resultados" ]
  , div_ [  ]
      [ vistaResultado "Prueba de Medias" (_resMedias (_resultados modelo))
      , vistaResultado "Prueba de Varianza" (_resVarianza (_resultados modelo))
      , vistaResultado "Prueba Chi-Cuadrada" (_resChiCuadrada (_resultados modelo))
      , vistaResultado "Prueba Kolmogorov-Smirnov" (_resKolmogorov (_resultados modelo))
      ]
  ]

-- Función auxiliar (UI Pura) para dibujar las tarjetas de resultados
vistaResultado :: MisoString -> Maybe ResultadoPrueba -> View model action
vistaResultado titulo Nothing = 
  div_ [  ] [ text (titulo <> ": Esperando datos...") ]
vistaResultado titulo (Just res) = 
  let mensaje    = if _pasaPrueba res then "Aprobado" else "Reprobado"
      esKS       = "Kolmogorov" `isInfixOf` fromMisoString titulo
      lblCritico :: String
      lblCritico = if esKS then "P-Valor: " else "Valor Crítico: "
  in div_ [ ]
       [ strong_ [] [ text titulo ]
       , p_ [] [ text ("Estadístico: " <> ms (show (_estadisticoCalculado res))) ]
       , p_ [] [ text (ms lblCritico <> ms (show (_valorTeorico res))) ]
       , p_ [] [ text mensaje ]
       ]

-- Helper para limpiar errores al guardar exitosamente
setTextoSinError :: MisoString -> InputValidado
setTextoSinError t = InputValidado t Nothing
