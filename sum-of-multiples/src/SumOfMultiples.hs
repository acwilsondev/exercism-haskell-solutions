module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum 
  [ m 
  | m <- [1..limit-1]
  , any (\f -> f > 0 && m `rem` f == 0) factors]

