module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number = addCountryCode . filter isDigit

not0Or1 :: Char -> Bool
not0Or1 c = c /= '0' && c /= '1'

addCountryCode :: String -> Maybe String
addCountryCode xs
  | length xs == 11 && xs !! 0 == '1' && not0Or1 (xs !! 1) && not0Or1 (xs !! 4) = Just $ tail xs
  | length xs == 10 && not0Or1 (xs !! 0) && not0Or1 (xs !! 3) = Just $ xs
  | otherwise = Nothing
