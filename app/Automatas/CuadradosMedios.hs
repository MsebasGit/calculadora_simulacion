{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.CuadradosMedios
  ( CuadradosMediosModel (..)
  , CuadradosMediosAction (..)
  , xcero
  , updateModel
  , viewModel
  , xn
  , semillaOriginal
  , historial
  ) where

import Miso
import Miso.Lens
import qualified Data.Vector as V
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H
import Miso.Html.Event (onInput, onClick)
import Miso.Html.Property

import Text.Read (readMaybe)

-- | Modelo local para el método de Cuadrados Medios
data CuadradosMediosModel = CuadradosMediosModel
  { _inputTemporal   :: MisoString
  , _errorValidacion :: Maybe MisoString  -- <--- NUEVO ESTADO
  , _semillaOriginal :: Maybe Int
  , _xn              :: Int
  , _historial       :: V.Vector Int
  } deriving (Show, Eq)

-- | Acciones locales para el método de Cuadrados Medios
data CuadradosMediosAction 
  = EscribirSemilla MisoString  -- Acción rápida de UI
  | FijarSemilla                -- Acción pesada de validación
  | Iterar
  | IterarN Int
  | Reiniciar
  deriving (Show, Eq)

-- | Lentes para manipular el modelo local
xn :: Lens CuadradosMediosModel Int
xn = lens _xn $ \record x -> record {_xn = x}

errorValidacion :: Lens CuadradosMediosModel (Maybe MisoString)
errorValidacion = lens _errorValidacion $ \record x -> record {_errorValidacion = x}

semillaOriginal :: Lens CuadradosMediosModel (Maybe Int)
semillaOriginal = lens _semillaOriginal $ \record x -> record {_semillaOriginal = x}

historial :: Lens CuadradosMediosModel (V.Vector Int)
historial = lens _historial $ \record x -> record {_historial = x}

-- | Estado inicial
xcero :: CuadradosMediosModel
xcero = CuadradosMediosModel "" Nothing Nothing 0  V.empty 

-- | Actualización de estado local (pure update)
updateModel :: CuadradosMediosAction -> CuadradosMediosModel -> CuadradosMediosModel
updateModel = \case
  EscribirSemilla texto -> \modelo -> 
    modelo { _inputTemporal = texto }

  FijarSemilla -> \modelo ->
  -- Intentamos leer el texto que estaba guardado en el modelo
    case readMaybe (fromMisoString (_inputTemporal modelo)) of
      Just nuevaSemilla -> 
         modelo { _xn = nuevaSemilla
                , _semillaOriginal = Just nuevaSemilla 
                }
      Nothing -> 
         modelo -- Si escribió letras, simplemente lo ignoramos (o podrías mostrar un error visual)
        
  Reiniciar -> \modelo ->
    case _semillaOriginal modelo of
      Nothing -> modelo
      Just s  -> modelo { _xn = s, _historial = V.empty }

  IterarN n -> \modelo ->
    let semillaActual = _xn modelo
        nuevosValores = V.tail $ V.iterateN (n + 1) F.cuadradosMedios semillaActual
    in if n <= 0 then modelo
       else modelo
         { _xn        = if V.null nuevosValores then semillaActual else V.last nuevosValores
         , _historial = _historial modelo V.++ nuevosValores
         }

  Iterar -> \modelo ->
    let semillaActual = _xn modelo
        nuevoValor    = F.cuadradosMedios semillaActual
    in modelo
      { _xn        = nuevoValor
      , _historial = _historial modelo `V.snoc` nuevoValor
      }


viewModel :: CuadradosMediosModel -> View model CuadradosMediosAction
viewModel modelo = H.div_ [ ]
  [ H.h2_ [] [ text "Generador: Cuadrados Medios" ]
  
  -- Mostramos el estado actual del generador
  , H.div_ [ ] 
      [ H.strong_ [] [ text "Semilla / Valor actual (Xn): " ]
      , text (ms (show (_xn modelo))) 
      ]
  
  -- Separamos los controles y la tabla en funciones más pequeñas
  , panelControles modelo
  , H.hr_ []
  , tablaHistorial (_historial modelo)
  ]

---
--- COMPONENTES DE LA INTERFAZ
---
{-
viewModel :: () -> Model -> View Model Action
viewModel _props modelo = div_ []
  [ h1_ [] [ text (_titulo modelo) ] 
  , input_ [ type_ "text", onInput CambiarTitulo ] 
  , br_ []
  , button_ [ onClick Restar ] [ text "-" ]
  , text (ms (show (_contador modelo)))
  , button_ [ onClick Sumar ] [ text "+" ]
  ]
-}

panelControles :: CuadradosMediosModel -> View model CuadradosMediosAction
panelControles modelo = H.div_ [ ]
  [ -- Nota de diseño: En una app real tendrías un campo de texto (input_)
    -- que actualice un estado temporal, y este botón leería ese estado.
    -- Para este ejemplo, inyectamos una semilla fija de 4 dígitos.
    H.input_  [ type_ "number", onInput EscribirSemilla ]  
  , H.button_ [ onClick FijarSemilla ] 
              [ text ("Pulse aquí para fijar la semilla: " <> _inputTemporal modelo)]
  , H.button_ [ onClick Iterar ] [ text "Iterar 1 vez" ]
  , H.button_ [ onClick (IterarN 10) ] [ text "Iterar 10 veces" ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar" ]
  ]

tablaHistorial :: V.Vector Int -> View model CuadradosMediosAction
tablaHistorial historialVec = 
  H.table_ []
    [ H.thead_ []
      [ H.tr_ []
        [ H.th_ [] [ text "n" ]
        , H.th_ [] [ text "Xn" ]
        ]
      ]
    , H.tbody_ [] filasHTML
    ]
  where
    -- 1. Convertimos el Vector a una lista estándar
    listaValores = V.toList historialVec
    
    -- 2. Numeramos los elementos (ej. [(1, 5731), (2, 8443), ...])
    listaNumerada = zip [1..] listaValores :: [(Int, Int)]
    
    -- 3. Mapeamos cada par a una fila de tabla <tr> usando "List Comprehension"
    filasHTML = [ H.tr_ [] 
                    [ H.td_ [  ] 
                        [ text (ms (show iteracion)) ]
                    , H.td_ [  ] 
                        [ text (ms (show valor)) ] 
                    ] 
                | (iteracion, valor) <- listaNumerada 
                ]                

