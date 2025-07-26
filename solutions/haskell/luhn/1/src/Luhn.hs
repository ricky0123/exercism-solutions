module Luhn (isValid) where

import Text.Read (readMaybe)

import qualified Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Data.List (singleton)
import Data.Char (isSpace)

isValid :: String -> Bool
isValid n = fromMaybe False $ fmap (\xs -> validLength xs && sumDivisibleBy10 xs) $ parse n

validLength :: [a] -> Bool
validLength xs = length xs > 1

parse :: String -> Maybe [Int]
parse = traverse readMaybe . map singleton . filter (not . isSpace)

sumDivisibleBy10 :: [Int] -> Bool
sumDivisibleBy10 xs = sum (doubleEveryOtherNumber xs) `mod` 10 == 0

doubleEveryOtherNumber :: [Int] -> [Int]
doubleEveryOtherNumber = reverse . zipWith f [0..] . reverse
  where
    f :: Int -> Int -> Int
    f i x
      | i `mod` 2 == 1 && 2 * x > 9 = 2 * x - 9
      | i `mod` 2 == 1 && 2 * x <= 9 = 2 * x
      | otherwise = x