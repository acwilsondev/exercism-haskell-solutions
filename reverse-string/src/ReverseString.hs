module ReverseString (reverseString) where

-- given a string, we need to return the reverse of the string.
-- e.g. "" -> "", "a" -> "a", "cat" -> "tac"
-- could just use `= reverse` but we'll do it the "hard way" for learning
reverseString :: String -> String
reverseString str = reverseStringR [] str

reverseStringR :: String -> String -> String
reverseStringR acc []     = acc
reverseStringR acc (x:xs) = reverseStringR (x:acc) xs

