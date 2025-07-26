module Pangram (isPangram) where

import Data.Char (toLower)

letters :: [Char]
letters = ['a'..'z']

isPangram :: String -> Bool
isPangram text = 
  and letterAppearances
  where
    letterAppearances = map inText letters
    inText c = c `elem` lowerText
    lowerText = map toLower text
