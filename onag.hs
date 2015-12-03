module ONAG where

data N = N [N] [N]
  deriving (Show)

geq :: N -> N -> Bool
-- Looks unnecessary
-- geq (N [] []) (N [] []) = True
geq x@(N xl xr) y@(N yl yr) = (not (any (y`geq`) xr)) && (not (any (`geq`x) yl))

eq :: N -> N -> Bool
eq x y = x`geq`y && y`geq`x

-- Numbers
zero = N [] []
one = N [zero] []
minus_one = N [] [zero]

-- Tests
tests = and [
    zero`eq`zero
  , one`geq`zero
  , zero`geq`minus_one
  ]


