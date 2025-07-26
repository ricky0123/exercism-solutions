module Prime (nth) where

import Data.List (unfoldr)

(!?) :: [a] -> Int -> Maybe a
(!?) xs i
  | (0<= i) && (i < l) = Just $ xs !! i
  | otherwise = Nothing
    where
      l = length $ take (i+1) xs

intToInteger :: Int -> Integer
intToInteger i = fromIntegral i

nth :: Int -> Maybe Integer
nth n = fmap intToInteger $ primes !? (n - 1)

primes = [2] <> unfoldr p' [2]

p' :: [Int] -> Maybe (Int, [Int])
p' ps = Just $ (nextPrime, ps <> [nextPrime])
  where
    start = ps !! (length ps - 1)
    nextPrime = p'' ps start

    p'' :: [Int] -> Int -> Int
    p'' ps i
      | any (hasFactor i) ps = p'' ps (i+1)
      | otherwise = i

hasFactor :: Int -> Int -> Bool
hasFactor x y = (x `mod` y) == 0
