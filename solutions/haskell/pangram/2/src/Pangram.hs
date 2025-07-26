module Pangram (isPangram) where

import Data.Char (toLower)

letters :: [Char]
letters = ['a'..'z']

isPangram :: String -> Bool
isPangram text =
  all inText letters
  where
    inText c = c `elem` lowerText
    lowerText = map toLower text
