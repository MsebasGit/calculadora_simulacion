{-# LANGUAGE OverloadedStrings #-}
module UI.Math
  ( xn        -- ^ X_n
  , x0        -- ^ X_0
  , x1        -- ^ X_1
  , xnp1      -- ^ X_{n+1}
  , indexn    -- ^ n
  , rn        -- ^ R_n
  , constc    -- ^ c
  , consta    -- ^ a
  , constm    -- ^ m
  ) where

import Miso
import qualified Miso.Mathml as M

-- | Renderiza X_n
xn :: View model action
xn =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mn_ [] [text "n"]
    ]
  ]

-- | Renderiza X_0
x0 :: View model action
x0 =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mn_ [] [text "0"]
    ]
  ]

-- | Renderiza X_1
x1 :: View model action
x1 =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mn_ [] [text "1"]
    ]
  ]

-- | Renderiza X_{n+1}
xnp1 :: View model action
xnp1 =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mrow_ [] [
        M.mi_ [] [text "n"],
        M.mo_ [] [text "+"],
        M.mn_ [] [text "1"]
      ]
    ]
  ]

-- | Renderiza n
indexn :: View model action
indexn =
  M.math_ [] [
    M.mi_ [] [text "n"]
  ]

-- | Renderiza R_n
rn :: View model action
rn =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "R"],
      M.mn_ [] [text "n"]
    ]
  ]

-- | Renderiza la constante c
constc :: View model action
constc =
  M.math_ [] [
    M.mi_ [] [text "c"]
  ]

-- | Renderiza la constante a
consta :: View model action
consta =
  M.math_ [] [
    M.mi_ [] [text "a"]
  ]

-- | Renderiza la constante m
constm :: View model action
constm =
  M.math_ [] [
    M.mi_ [] [text "m"]
  ]
