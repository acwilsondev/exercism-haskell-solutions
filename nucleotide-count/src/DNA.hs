module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = (fromListWith (+) <$>) . traverse f
  where 
    f 'A' = Right (A, 1)
    f 'C' = Right (C, 1)
    f 'G' = Right (G, 1)
    f 'T' = Right (T, 1)
    f c   = Left [c]

