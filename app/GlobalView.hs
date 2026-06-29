{-# LANGUAGE OverloadedStrings #-}
module GlobalView
  ( viewModel
  ) where

import Miso
import qualified Miso.Html as H
import GlobalTypes
import qualified Automatas.CuadradosMedios as CM
import qualified Automatas.Congruencial as C
import qualified Automatas.CongruencialMult as CMul
import qualified Automatas.PruebasEstadisticas as PE
import qualified Automatas.MultiplicadorConstante as MC
import qualified Automatas.ProductosMedios as PM
import qualified Automatas.MersenneTwister as MT
import qualified Automatas.Ruleta as R
import UI.Navbar (viewMainNavbar, viewSubNavbar)
import Miso.Html.Property (class_, id_, src_, loop_, preload_)

-- | Renderizado principal de la interfaz global
viewModel :: () -> Model -> View Model Action
viewModel _ modelo = H.div_ [ class_ "app-container" ]
  [ H.h1_ [ class_ "app-title" ] [ text "Calculadora de Simulación (Autómata Principal)" ]
  
  -- Navbar Principal
  , viewMainNavbar (_seccionActiva modelo)
  , H.hr_ [ class_ "divider" ]
  
  -- Caja principal con animación fade-in
  , H.main_ [ class_ "fade-in" ]
      [ case _seccionActiva modelo of
          SeccionPseudoaleatorios ->
            H.div_ []
               [ -- Navbar Secundaria (sólo para pseudoaleatorios)
                 viewSubNavbar (_activeTab modelo)
               , H.hr_ [ class_ "divider" ]
               , case _activeTab modelo of
                   TabCuadradosMedios ->
                     fmap AccionCuadradosMedios (CM.viewModel (_cuadradosMedios modelo))
                   TabCongruencial ->
                     fmap AccionCongruencial (C.viewModel (_congruencial modelo))
                   TabCongruencialMult ->
                     fmap AccionCongruencialMult (CMul.viewModel (_congruencialMult modelo))
                   TabPruebasEstadisticas ->
                     fmap AccionPruebasEstadisticas (PE.viewModel (_pruebasEstadisticas modelo))
                   TabMultiplicadorConstante ->
                     fmap AccionMultiplicadorConstante (MC.viewModel (_multiplicadorConstante modelo))
                   TabProductosMedios ->
                     fmap AccionProductosMedios (PM.viewModel (_productosMedios modelo))
                   TabMersenneTwister ->
                     fmap AccionMersenneTwister (MT.viewModel (_mersenneTwister modelo))
               ]

          SeccionRuleta ->
            fmap AccionRuleta (R.viewModel (_ruleta modelo))

          SeccionCovid ->
            H.div_ [ class_ "card fade-in placeholder-card" ]
              [ H.h2_ [ class_ "card-title" ] [ text "Autómata Celular COVID-19" ]
              , H.p_ [ class_ "card-desc" ]
                  [ text "Próximamente: Simulación de la propagación del virus utilizando autómatas celulares." ]
              ]
      ]
  , H.audio_ [ id_ "sound-click", src_ "PonerFichas.mp3", preload_ "auto" ] []
  , H.audio_ [ id_ "sound-spin", src_ "BolitaGirando.mp3", preload_ "auto" ] []
  , H.audio_ [ id_ "sound-win", src_ "WinSFX.mp3", preload_ "auto" ] []
  , H.audio_ [ id_ "sound-lose", src_ "Failure.mp3", preload_ "auto" ] []
  , H.audio_ [ id_ "sound-music", src_ "CasinoMusic.mp3", loop_ True, preload_ "auto" ] []
  ]
