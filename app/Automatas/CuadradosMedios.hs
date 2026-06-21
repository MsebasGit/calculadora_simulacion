{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.CuadradosMedios
  ( CuadradosMediosModel (..)
  , CuadradosMediosAction (..)
  , xcero
  , updateModel
  , viewModel
  , xn
  , inputSemilla
  , inputIteraciones
  , semillaOriginal
  , historial
  ) where

import Miso
import Miso.Lens
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Set as S
import SubAutomatas.InputValidado
import qualified UI.Math as UM


-- | Modelo local para el método de Cuadrados Medios
data CuadradosMediosModel = CuadradosMediosModel
  { _inputSemilla      :: InputValidado
  , _inputIteraciones  :: InputValidado
  , _semillaOriginal   :: Maybe Int
  , _xn                :: Int
  , _historial         :: [Int]
  } deriving (Show, Eq)

-- | Acciones locales para el método de Cuadrados Medios
data CuadradosMediosAction 
  = AccionInputSemilla InputValidadoAction
  | AccionInputIteraciones InputValidadoAction
  | FijarSemilla                -- Acción pesada de validación
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  deriving (Show, Eq)

-- | Lentes para manipular el modelo local
xn :: Lens CuadradosMediosModel Int
xn = lens _xn $ \record x -> record {_xn = x}

inputSemilla :: Lens CuadradosMediosModel InputValidado
inputSemilla = lens _inputSemilla $ \record x -> record {_inputSemilla = x}

inputIteraciones :: Lens CuadradosMediosModel InputValidado
inputIteraciones = lens _inputIteraciones $ \record x -> record {_inputIteraciones = x}

semillaOriginal :: Lens CuadradosMediosModel (Maybe Int)
semillaOriginal = lens _semillaOriginal $ \record x -> record {_semillaOriginal = x}

historial :: Lens CuadradosMediosModel [Int]
historial = lens _historial $ \record x -> record {_historial = x}

-- | Estado inicial
xcero :: CuadradosMediosModel
xcero = CuadradosMediosModel (InputValidado "" Nothing) (InputValidado "10" Nothing) Nothing 0 [] 

-- | Actualización de estado local (pure update)
updateModel :: CuadradosMediosAction -> CuadradosMediosModel -> CuadradosMediosModel
updateModel action modelo = case action of
  AccionInputSemilla subAct ->
    inputSemilla %~ updateInputValidado subAct $ modelo

  AccionInputIteraciones subAct ->
    inputIteraciones %~ updateInputValidado subAct $ modelo

  FijarSemilla ->
    let str = fromMisoString (_textoTemporal (_inputSemilla modelo))
    in case readMaybe str of
         Just nuevaSemilla
           | nuevaSemilla >= 100 ->
               let modificado = modelo
                     { _xn = nuevaSemilla
                     , _semillaOriginal = Just nuevaSemilla
                     }
               in inputSemilla %~ (errorActual .~ Nothing) $ modificado
           | otherwise ->
               inputSemilla %~ (errorActual ?~ "La semilla debe ser mayor a 100") $ modelo
         Nothing ->
           inputSemilla %~ (errorActual ?~ "La semilla debe ser un número entero válido") $ modelo
        
  Reiniciar ->
    case _semillaOriginal modelo of
      Nothing -> modelo
      Just s  -> modelo { _semillaOriginal = Nothing, _xn = s, _historial = [] }

  IterarNUsuario ->
    let str = fromMisoString (_textoTemporal (_inputIteraciones modelo))
    in case readMaybe str of
         Just n
           | n > 0 ->
               let semillaActual = _xn modelo
                   nuevosValores = drop 1 $ take (n + 1) $ iterate F.cuadradosMedios semillaActual
                   modeloConHistorial = modelo
                     { _xn        = if null nuevosValores then semillaActual else last nuevosValores
                     , _historial = reverse nuevosValores ++ _historial modelo
                     }
               in inputIteraciones %~ (errorActual .~ Nothing) $ modeloConHistorial
           | otherwise ->
               inputIteraciones %~ (errorActual ?~ "Debe ingresar al menos 1 iteración") $ modelo
         Nothing ->
           inputIteraciones %~ (errorActual ?~ "Ingrese un número entero válido") $ modelo

  IterarN n ->
    let semillaActual = _xn modelo
        nuevosValores = drop 1 $ take (n + 1) $ iterate F.cuadradosMedios semillaActual
    in if n <= 0 then modelo
       else modelo
         { _xn        = if null nuevosValores then semillaActual else last nuevosValores
         , _historial = reverse nuevosValores ++ _historial modelo
         }

  Iterar ->
    let semillaActual = _xn modelo
        nuevoValor    = F.cuadradosMedios semillaActual
    in modelo
      { _xn        = nuevoValor
      , _historial = nuevoValor : _historial modelo
      }


viewModel :: CuadradosMediosModel -> View model CuadradosMediosAction
viewModel modelo = H.div_ [ ]
  [ H.h2_ [] [ text "Generador: Cuadrados Medios" ]
  
  -- Mostramos el estado actual del generador
  , H.div_ [] 
      [ H.strong_ [] [ text "Semilla original (", UM.x0, text "): " ]
      , text (case _semillaOriginal modelo of
                 Nothing -> "No establecida"
                 Just s0 -> ms (show s0))
      ]
  , H.div_ [ ] 
      [ H.strong_ [] [ text "Valor actual (", UM.xn, text "): " ]
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
  [ mostrarControles ]
  where
    mostrarControles =
      case _semillaOriginal modelo of
        Nothing -> H.div_ [] 
          [ H.div_ [] [ text "Semilla (", UM.x0, text "):" ]
          , viewInputValidado AccionInputSemilla (_inputSemilla modelo)
          , H.button_ [ onClick FijarSemilla ]
                      [ text ("Pulse aquí para fijar la semilla: " <> _textoTemporal (_inputSemilla modelo))]
          ]
        Just _  ->
          let cicloDetectado = tieneDuplicados (_historial modelo)
          in controlesSimulacion cicloDetectado modelo

-- | Retorna True si el historial contiene elementos duplicados
tieneDuplicados :: [Int] -> Bool
tieneDuplicados lista =
  length lista /= S.size (S.fromList lista)

-- | Renderiza los controles de simulación (si cicloDetectado es True, deshabilita las iteraciones)
controlesSimulacion :: Bool -> CuadradosMediosModel -> View model CuadradosMediosAction
controlesSimulacion True _ = H.div_ []
  [ H.div_ [ class_ "warning-message" ] 
           [ text "¡Ciclo detectado! La secuencia ha comenzado a repetirse. Simulación detenida." ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar" ]
  ]
controlesSimulacion False modelo = H.div_ []
  [ H.button_ [ onClick Iterar ] [ text "Iterar 1 vez" ]
  , H.button_ [ onClick (IterarN 10) ] [ text "Iterar 10 veces" ]
  , H.hr_ []
  , H.div_ [] [ text "Cantidad personalizada de iteraciones:" ]
  , viewInputValidado AccionInputIteraciones (_inputIteraciones modelo)
  , H.button_ [ onClick IterarNUsuario ]
              [ text ("Iterar " <> _textoTemporal (_inputIteraciones modelo) <> " veces") ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar" ]
  ]

tablaHistorial :: [Int] -> View model CuadradosMediosAction
tablaHistorial historialList = 
  H.table_ []
    [ H.thead_ []
      [ H.tr_ []
        [ H.th_ [] [ UM.indexn ]
        , H.th_ [] [ UM.xn ]
        , H.th_ [] [ UM.rn ]
        ]
      ]
    , H.tbody_ [] filasHTML
    ]
  where
    -- 1. Revertimos la lista para que se muestre en orden original
    listaValores = reverse historialList
    listaPseudo  = map F.pseudoaleatorioNC listaValores
        
    -- 2. Numeramos los elementos (ej. [(1, 5731), (2, 8443), ...])
    listaNumerada = zip3 [1..] listaValores listaPseudo :: [(Int, Int, Float)]

    -- 3. Mapeamos cada par a una fila de tabla <tr> usando "List Comprehension"
    filasHTML = [ H.tr_ [] 
                    [ H.td_ [  ] 
                        [ text (ms (show iteracion)) ]
                    , H.td_ [  ] 
                        [ text (ms (show valor)) ] 
                    , H.td_ [  ] 
                        [ text (ms (show pseudo)) ] 
                    ] 
                | (iteracion, valor, pseudo) <- listaNumerada 
                ]                

