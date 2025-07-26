module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fmap countValues nucleotides
  where
    parseNucleotide :: Char -> Either String Nucleotide
    parseNucleotide x = case x of
      'A' -> Right A
      'C' -> Right C
      'G' -> Right G
      'T' -> Right T
      _ -> Left "Invalid character"
      
    nucleotides :: Either String [Nucleotide]
    nucleotides = traverse parseNucleotide xs

    countValues :: (Ord a) => [a] -> Map a Int
    countValues = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty
