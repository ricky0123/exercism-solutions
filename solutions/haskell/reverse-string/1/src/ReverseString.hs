module ReverseString (reverseString) where

reverseString :: String -> String
reverseString [] = []
reverseString (first:rest) = reverseString rest ++ [first]
