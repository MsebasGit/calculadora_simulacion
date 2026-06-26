{-# LANGUAGE OverloadedStrings #-}
module UI.Table
  ( tablaPaginada
  , elementosPorPagina
  , calcularMaxPagina
  ) where

import Miso
import qualified Miso.Html as H
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_, disabled_)

-- | Cantidad de elementos mostrados por página
elementosPorPagina :: Int
elementosPorPagina = 20

-- | Calcula el número máximo de páginas para una cantidad de elementos dada
calcularMaxPagina :: Int -> Int
calcularMaxPagina total = max 1 ((total + elementosPorPagina - 1) `div` elementosPorPagina)

-- | Renderiza una tabla paginada genérica de forma altamente eficiente,
-- realizando el slicing de elementos antes de procesar o revertir la lista.
tablaPaginada 
  :: [View model action]               -- ^ Cabeceras de la tabla
  -> [item]                            -- ^ Historial completo (con el elemento más nuevo al inicio)
  -> Int                               -- ^ Página actual (1-indexada)
  -> (Int -> item -> [View model action]) -- ^ Función que toma el índice cronológico (1-indexado) y el item, retornando sus celdas
  -> action                            -- ^ Acción para retroceder de página
  -> action                            -- ^ Acción para avanzar de página
  -> View model action
tablaPaginada cabeceras historialList paginaActual renderFila accionAnt accionSig =
  H.div_ []
    [ H.div_ [ class_ "table-responsive" ]
        [ H.table_ []
            [ H.thead_ []
                [ H.tr_ [] [ H.th_ [] [h] | h <- cabeceras ] ]
            , H.tbody_ [] filasHTML
            ]
        ]
    , H.div_ [ class_ "pagination-controls" ]
        [ H.button_ ( onClick accionAnt : [ disabled_ | paginaActual <= 1 ]) [ text "Anterior" ]
        , H.span_ [] [ text (ms (" Página " ++ show paginaActual ++ " de " ++ show maxPagina ++ " ")) ]
        , H.button_ ( onClick accionSig : [ disabled_ | paginaActual >= maxPagina ]) [ text "Siguiente" ]
        ]
    ]
  where
    totalElementos = length historialList
    maxPagina = calcularMaxPagina totalElementos

    -- Slicing eficiente sobre la lista:
    startIndex = (paginaActual - 1) * elementosPorPagina
    endIndex = min totalElementos (paginaActual * elementosPorPagina)
    numElementos = endIndex - startIndex

    -- Los elementos cronológicos deseados están al final de la lista invertida
    valoresPaginadosReversados = take numElementos $ drop (totalElementos - endIndex) historialList
    valoresPaginados = reverse valoresPaginadosReversados

    -- Mapeamos cada celda llamando a la función renderFila proveída
    filasHTML = 
      [ H.tr_ [] [ H.td_ [] [celda] | celda <- renderFila idx val ]
      | (idx, val) <- zip [startIndex + 1 ..] valoresPaginados
      ]

