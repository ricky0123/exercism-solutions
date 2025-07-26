module Pangram (isPangram) where

import Data.Char (toLower)

letters :: [Char]
letters = ['a'..'z']

isPangram :: String -> Bool
isPangram text =
  and [c `elem` lowerText | c <- letters]
  where
    lowerText = map toLower text
