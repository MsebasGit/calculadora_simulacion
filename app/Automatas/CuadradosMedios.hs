{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , paginaActual
  , analizador
  ) where

import Miso
import Control.Lens
import qualified Funciones.Aleatorios as F
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)

import Text.Read (readMaybe)
import qualified Data.Set as S
import SubAutomatas.InputValidado
import qualified UI.Math as UM
import qualified UI.Table as UT
import qualified Data.Vector as V
import SubAutomatas.AnalizadorEstadistico


-- | Modelo local para el método de Cuadrados Medios
data CuadradosMediosModel = CuadradosMediosModel
  { _inputSemilla      :: InputValidado
  , _inputIteraciones  :: InputValidado
  , _semillaOriginal   :: Maybe Int
  , _xn                :: Int
  , _historial         :: [Int]
  , _paginaActual      :: Int
  , _analizador        :: AnalizadorModel
  } deriving (Show, Eq)

makeLenses ''CuadradosMediosModel

-- | Acciones locales para el método de Cuadrados Medios
data CuadradosMediosAction 
  = AccionInputSemilla InputValidadoAction
  | AccionInputIteraciones InputValidadoAction
  | FijarSemilla                -- Acción pesada de validación
  | Iterar
  | IterarNUsuario
  | IterarN Int
  | Reiniciar
  | PaginaAnterior
  | PaginaSiguiente
  | AccionAnalizador AnalizadorAction
  deriving (Show, Eq)

-- | Estado inicial
xcero :: CuadradosMediosModel
xcero = CuadradosMediosModel (InputValidado "" Nothing) (InputValidado "10" Nothing) Nothing 0 [] 1 analizadorInicial 

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
      Just s  -> modelo { _semillaOriginal = Nothing, _xn = s, _historial = [], _paginaActual = 1 }

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
                     , _paginaActual = 1
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
         , _paginaActual = 1
         }

  Iterar ->
    let semillaActual = _xn modelo
        nuevoValor    = F.cuadradosMedios semillaActual
    in modelo
      { _xn        = nuevoValor
      , _historial = nuevoValor : _historial modelo
      , _paginaActual = 1
      }

  PaginaAnterior ->
    if _paginaActual modelo > 1
      then modelo { _paginaActual = _paginaActual modelo - 1 }
      else modelo

  PaginaSiguiente ->
    let totalElementos = length (_historial modelo)
        maxPagina = UT.calcularMaxPagina totalElementos
    in if _paginaActual modelo < maxPagina
         then modelo { _paginaActual = _paginaActual modelo + 1 }
         else modelo

  AccionAnalizador subAct ->
    analizador %~ updateAnalizador subAct $ modelo



viewModel :: CuadradosMediosModel -> View model CuadradosMediosAction
viewModel modelo = H.div_ []
  [ H.h2_ [] 
      [ text "Generador: Cuadrados Medios "
      , H.span_ [ class_ "formula-title" ] [ UM.formulaCuadradosMedios ]
      ]
  
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
  , case _semillaOriginal modelo of
      Nothing -> H.div_ [] []
      Just _  -> H.div_ []
         [ tablaHistorial (_paginaActual modelo) (_historial modelo)
         , if null (_historial modelo)
             then H.div_ [] []
             else H.div_ [ class_ "card fade-in" ]
               [ H.h3_ [] [ text "Pruebas Estadísticas" ]
               , H.button_ [ onClick (AccionAnalizador (EjecutarPruebas (V.fromList (map (realToFrac . F.pseudoaleatorioNC) (_historial modelo))))), class_ "btn-primary" ]
                   [ text "Ejecutar Pruebas Estadísticas" ]
               , H.hr_ []
               , fmap AccionAnalizador (viewAnalizador (_analizador modelo))
               ]
         ]
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
          , H.button_ [ onClick FijarSemilla, class_ "btn-primary" ]
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

tablaHistorial :: Int -> [Int] -> View model CuadradosMediosAction
tablaHistorial pagAct historialList =
  UT.tablaPaginada
    [ UM.indexn, UM.xn, UM.rn ]
    historialList
    pagAct
    (\idx valor ->
       [ text (ms (show idx))
       , text (ms (show valor))
       , text (ms (show (F.pseudoaleatorioNC valor)))
       ]
    )
    PaginaAnterior
    PaginaSiguiente



