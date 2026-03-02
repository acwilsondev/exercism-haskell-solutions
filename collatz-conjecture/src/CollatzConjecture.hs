module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n 
  | n <= 0 = Nothing
  | otherwise = Just (go 0 n)
  where
    go steps 1 = steps
    go steps num
      | even num  = go (steps + 1) (num `div` 2)
      | otherwise = go (steps + 1) (3 * num + 1)

