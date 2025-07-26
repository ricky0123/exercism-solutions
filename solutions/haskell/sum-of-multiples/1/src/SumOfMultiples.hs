module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [i| i <- [1..(limit - 1)], any (\factor -> i `mod` factor == 0) factors']
  where
    factors' = filter ((/=) 0) factors
