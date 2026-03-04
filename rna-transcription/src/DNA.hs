module DNA (toRNA) where

-- Traverse a string, converting DNA to RNA
toRNA :: String -> Either Char String
toRNA = traverse dnaToRna

dnaToRna :: Char -> Either Char Char
dnaToRna 'G' = Right 'C'
dnaToRna 'C' = Right 'G'
dnaToRna 'T' = Right 'A'
dnaToRna 'A' = Right 'U'
dnaToRna x   = Left x
