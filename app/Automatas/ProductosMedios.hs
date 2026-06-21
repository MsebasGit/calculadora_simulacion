{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Automatas.ProductosMedios
  ( ProductosMediosModel (..)
  , ProductosMediosAction (..)
  , xcero
  , updateModel
  , viewModel
  , xn0
  , xn1
  , inputSemilla0
  , inputSemilla1
  , inputIteraciones
  , parametrosOriginales
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
import UI.Math


-- | Modelo local para el método de Productos Medios
data ProductosMediosModel = ProductosMediosModel
  { _inputSemilla0        :: InputValidado
  , _inputSemilla1        :: InputValidado
  , _inputIteraciones     :: InputValidado
  , _parametrosOriginales :: Maybe (Int, Int) -- (x0, x1)
  , _xn0                  :: Int
  , _xn1                  :: Int
  , _historial            :: [Int]
  } deriving (Show, Eq)

-- | Acciones locales para el método de Productos Medios
data ProductosMediosAction
  = AccionInputSemilla0 InputValidadoAction
  | AccionInputSemilla1 InputValidadoAction
  | AccionInputIteraciones InputValidadoAction
  | FijarParametros
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  deriving (Show, Eq)

-- | Lentes para manipular el modelo local
xn0 :: Lens ProductosMediosModel Int
xn0 = lens _xn0 $ \record x -> record {_xn0 = x}

xn1 :: Lens ProductosMediosModel Int
xn1 = lens _xn1 $ \record x -> record {_xn1 = x}

inputSemilla0 :: Lens ProductosMediosModel InputValidado
inputSemilla0 = lens _inputSemilla0 $ \record x -> record {_inputSemilla0 = x}

inputSemilla1 :: Lens ProductosMediosModel InputValidado
inputSemilla1 = lens _inputSemilla1 $ \record x -> record {_inputSemilla1 = x}

inputIteraciones :: Lens ProductosMediosModel InputValidado
inputIteraciones = lens _inputIteraciones $ \record x -> record {_inputIteraciones = x}

parametrosOriginales :: Lens ProductosMediosModel (Maybe (Int, Int))
parametrosOriginales = lens _parametrosOriginales $ \record x -> record {_parametrosOriginales = x}

historial :: Lens ProductosMediosModel [Int]
historial = lens _historial $ \record x -> record {_historial = x}

-- | Estado inicial
xcero :: ProductosMediosModel
xcero = ProductosMediosModel
  (InputValidado "" Nothing)
  (InputValidado "" Nothing)
  (InputValidado "10" Nothing)
  Nothing 0 0 []

-- | Actualización de estado local (pure update)
updateModel :: ProductosMediosAction -> ProductosMediosModel -> ProductosMediosModel
updateModel action modelo = case action of
  AccionInputSemilla0 subAct ->
    inputSemilla0 %~ updateInputValidado subAct $ modelo

  AccionInputSemilla1 subAct ->
    inputSemilla1 %~ updateInputValidado subAct $ modelo

  AccionInputIteraciones subAct ->
    inputIteraciones %~ updateInputValidado subAct $ modelo

  FijarParametros ->
    let vS0 = readMaybe (fromMisoString (_textoTemporal (_inputSemilla0 modelo)))
        vS1 = readMaybe (fromMisoString (_textoTemporal (_inputSemilla1 modelo)))
    in case (vS0, vS1) of
         (Just s0, Just s1)
           | s0 >= 100 && s1 >= 100 ->
               let modificado = modelo
                     { _xn0 = s0
                     , _xn1 = s1
                     , _parametrosOriginales = Just (s0, s1)
                     }
                   sem0Limpia = inputSemilla0 %~ (errorActual .~ Nothing) $ modificado
               in inputSemilla1 %~ (errorActual .~ Nothing) $ sem0Limpia
           | otherwise ->
               let e0 = if s0 >= 100
                          then inputSemilla0 %~ (errorActual .~ Nothing) $ modelo
                          else inputSemilla0 %~ (errorActual ?~ "La semilla 1 debe ser mayor o igual a 100") $ modelo
                   e1 = if s1 >= 100
                          then inputSemilla1 %~ (errorActual .~ Nothing) $ e0
                          else inputSemilla1 %~ (errorActual ?~ "La semilla 2 debe ser mayor o igual a 100") $ e0
               in e1
         (mS0, mS1) ->
           let e0 = case mS0 of
                      Just s0 | s0 >= 100 -> inputSemilla0 %~ (errorActual .~ Nothing) $ modelo
                              | otherwise -> inputSemilla0 %~ (errorActual ?~ "La semilla 1 debe ser >= 100") $ modelo
                      Nothing -> inputSemilla0 %~ (errorActual ?~ "Ingrese un entero válido") $ modelo
               e1 = case mS1 of
                      Just s1 | s1 >= 100 -> inputSemilla1 %~ (errorActual .~ Nothing) $ e0
                              | otherwise -> inputSemilla1 %~ (errorActual ?~ "La semilla 2 debe ser >= 100") $ e0
                      Nothing -> inputSemilla1 %~ (errorActual ?~ "Ingrese un entero válido") $ e0
           in e1

  Reiniciar ->
    modelo
      { _parametrosOriginales = Nothing
      , _xn0 = 0
      , _xn1 = 0
      , _historial = []
      }

  IterarNUsuario ->
    let str = fromMisoString (_textoTemporal (_inputIteraciones modelo))
    in case (readMaybe str :: Maybe Int) of
         Just n
           | n > 0 ->
               let iterarNVeces 0 (curr0, curr1) hist = ((curr0, curr1), hist)
                   iterarNVeces k (curr0, curr1) hist =
                     let (next0, next1) = F.productosMedios (curr0, curr1)
                     in iterarNVeces (k - 1) (next0, next1) (next1 : hist)
                   ((fin0, fin1), nuevosValores) = iterarNVeces n (_xn0 modelo, _xn1 modelo) []
                   modeloConHistorial = modelo
                     { _xn0       = fin0
                     , _xn1       = fin1
                     , _historial = nuevosValores ++ _historial modelo
                     }
               in inputIteraciones %~ (errorActual .~ Nothing) $ modeloConHistorial
           | otherwise ->
               inputIteraciones %~ (errorActual ?~ "Debe ingresar al menos 1 iteración") $ modelo
         Nothing ->
           inputIteraciones %~ (errorActual ?~ "Ingrese un número entero válido") $ modelo

  IterarN n ->
    if n <= 0 then modelo
    else
      let iterarNVeces 0 (curr0, curr1) hist = ((curr0, curr1), hist)
          iterarNVeces k (curr0, curr1) hist =
            let (next0, next1) = F.productosMedios (curr0, curr1)
            in iterarNVeces (k - 1) (next0, next1) (next1 : hist)
          ((fin0, fin1), nuevosValores) = iterarNVeces n (_xn0 modelo, _xn1 modelo) []
      in modelo
        { _xn0       = fin0
        , _xn1       = fin1
        , _historial = nuevosValores ++ _historial modelo
        }

  Iterar ->
    let (next0, next1) = F.productosMedios (_xn0 modelo, _xn1 modelo)
    in modelo
      { _xn0       = next0
      , _xn1       = next1
      , _historial = next1 : _historial modelo
      }

-- | Renderizado visual del autómata Productos Medios
viewModel :: ProductosMediosModel -> View model ProductosMediosAction
viewModel modelo = H.div_ []
  [ H.h2_ [] [ text "Generador de Productos Medios" ]
  
  , H.div_ [] 
      [ H.strong_ [] [ text "Semilla 1 original (", x0, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (s0, _) -> ms (show s0))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Semilla 2 original (", x1, text "): " ]
      , text (case _parametrosOriginales modelo of
                 Nothing -> "No establecida"
                 Just (_, s1) -> ms (show s1))
      ]
  , H.div_ [] 
      [ H.strong_ [] [ text "Valores actuales (", xn, text ", ", xnp1, text "): " ]
      , text (ms ("(" ++ show (_xn0 modelo) ++ ", " ++ show (_xn1 modelo) ++ ")")) 
      ]
  
  , panelControles modelo
  , H.hr_ []
  , tablaHistorial (_historial modelo)
  ]

panelControles :: ProductosMediosModel -> View model ProductosMediosAction
panelControles modelo = H.div_ []
  [ mostrarControles ]
  where
    mostrarControles =
      case _parametrosOriginales modelo of
        Just _ -> 
          let cicloDetectado = tieneDuplicados (_historial modelo)
          in controlesSimulacion cicloDetectado modelo
        Nothing -> H.div_ []
          [ H.div_ [] [ text "Semilla 1 (", x0, text "):" ]
          , viewInputValidado AccionInputSemilla0 (_inputSemilla0 modelo)
          , H.hr_ []
          , H.div_ [] [ text "Semilla 2 (", x1, text "):" ]
          , viewInputValidado AccionInputSemilla1 (_inputSemilla1 modelo)
          , H.hr_ []
          , H.button_ [ onClick FijarParametros ]
                      [ text "Fijar Semillas" ]
          ]

tieneDuplicados :: [Int] -> Bool
tieneDuplicados lista =
  length lista /= S.size (S.fromList lista)

controlesSimulacion :: Bool -> ProductosMediosModel -> View model ProductosMediosAction
controlesSimulacion True _ = H.div_ []
  [ H.div_ [ class_ "warning-message" ] 
           [ text "¡Ciclo detectado! La secuencia ha comenzado a repetirse. Simulación detenida." ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar / Cambiar parámetros" ]
  ]
controlesSimulacion False modelo = H.div_ []
  [ H.button_ [ onClick Iterar ] [ text "Iterar 1 vez" ]
  , H.button_ [ onClick (IterarN 10) ] [ text "Iterar 10 veces" ]
  , H.hr_ []
  , H.div_ [] [ text "Cantidad personalizada de iteraciones:" ]
  , viewInputValidado AccionInputIteraciones (_inputIteraciones modelo)
  , H.button_ [ onClick IterarNUsuario ]
              [ text ("Iterar " <> _textoTemporal (_inputIteraciones modelo) <> " veces") ]
  , H.button_ [ onClick Reiniciar ] [ text "Reiniciar / Cambiar parámetros" ]
  ]

tablaHistorial :: [Int] -> View model ProductosMediosAction
tablaHistorial historialList = 
  H.table_ []
    [ H.thead_ []
      [ H.tr_ []
        [ H.th_ [] [ indexn ]
        , H.th_ [] [ xn ]
        , H.th_ [] [ rn ]
        ]
      ]
    , H.tbody_ [] filasHTML
    ]
  where
    listaValores = reverse historialList
    listaPseudo  = map F.pseudoaleatorioNC listaValores
    listaNumerada = zip3 [1..] listaValores listaPseudo :: [(Int, Int, Float)]
    filasHTML = [ H.tr_ [] 
                    [ H.td_ [] [ text (ms (show iteracion)) ]
                    , H.td_ [] [ text (ms (show valor)) ] 
                    , H.td_ [] [ text (ms (show pseudo)) ] 
                    ] 
                | (iteracion, valor, pseudo) <- listaNumerada 
                ]
