module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)
import Data.Function (on)
import Control.Applicative (liftA2)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (liftA2 (&&) (isAnagramFor xs) (notSame xs))
  where
    notSame = (/=) `on` map toLower

isAnagramFor :: String -> String -> Bool
isAnagramFor = (==) `on` _standard
  where
    _standard :: String -> String
    _standard = sort . map toLower