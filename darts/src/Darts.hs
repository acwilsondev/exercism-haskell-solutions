module Darts (score) where

-- return the points earned given a placement on a dartboard:
-- <= 1.0 -> 10 pts
-- <= 5.0 -> 5 pts
-- <= 10.0 -> 1 pt
-- otherwise -> 0 pts
score :: Float -> Float -> Int
score x y 
 | r <= 1.0   = 10
 | r <= 5.0   = 5
 | r <= 10.0  = 1
 | otherwise    = 0
 where 
  r = sqrt (x * x + y * y)

