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
  , formulaCuadradosMedios
  , formulaProductosMedios
  , formulaMultiplicadorConstante
  , formulaCongruencial
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

-- | Fórmula: X_{n+1} = X_n^2
formulaCuadradosMedios :: View model action
formulaCuadradosMedios =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mrow_ [] [
        M.mi_ [] [text "n"],
        M.mo_ [] [text "+"],
        M.mn_ [] [text "1"]
      ]
    ],
    M.mo_ [] [text "="],
    M.msup_ [] [
      M.msub_ [] [
        M.mi_ [] [text "X"],
        M.mi_ [] [text "n"]
      ],
      M.mn_ [] [text "2"]
    ]
  ]

-- | Fórmula: X_{n+2} = X_n * X_{n+1}
formulaProductosMedios :: View model action
formulaProductosMedios =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mrow_ [] [
        M.mi_ [] [text "n"],
        M.mo_ [] [text "+"],
        M.mn_ [] [text "2"]
      ]
    ],
    M.mo_ [] [text "="],
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mi_ [] [text "n"]
    ],
    M.mo_ [] [text "·"],
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mrow_ [] [
        M.mi_ [] [text "n"],
        M.mo_ [] [text "+"],
        M.mn_ [] [text "1"]
      ]
    ]
  ]

-- | Fórmula: X_{n+1} = c * X_n
formulaMultiplicadorConstante :: View model action
formulaMultiplicadorConstante =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mrow_ [] [
        M.mi_ [] [text "n"],
        M.mo_ [] [text "+"],
        M.mn_ [] [text "1"]
      ]
    ],
    M.mo_ [] [text "="],
    M.mi_ [] [text "c"],
    M.mo_ [] [text "·"],
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mi_ [] [text "n"]
    ]
  ]

-- | Fórmula: X_{n+1} = (a * X_n + c) mod m
formulaCongruencial :: View model action
formulaCongruencial =
  M.math_ [] [
    M.msub_ [] [
      M.mi_ [] [text "X"],
      M.mrow_ [] [
        M.mi_ [] [text "n"],
        M.mo_ [] [text "+"],
        M.mn_ [] [text "1"]
      ]
    ],
    M.mo_ [] [text "="],
    M.mrow_ [] [
      M.mo_ [] [text "("],
      M.mi_ [] [text "a"],
      M.mo_ [] [text "·"],
      M.msub_ [] [
        M.mi_ [] [text "X"],
        M.mi_ [] [text "n"]
      ],
      M.mo_ [] [text "+"],
      M.mi_ [] [text "c"],
      M.mo_ [] [text ")"],
      M.mo_ [] [text "mod"],
      M.mi_ [] [text "m"]
    ]
  ]
