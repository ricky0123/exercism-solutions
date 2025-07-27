module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify m
  | m <= 0 = Nothing
  | m == 1 = Just Deficient
  | aliquotSum < m = Just Deficient
  | aliquotSum == m = Just Perfect
  | aliquotSum > m = Just Abundant
  where
    aliquotSum = aliquot m

aliquot :: Int -> Int
aliquot i = sum $ quotients i

quotients :: Int -> [Int]
quotients m = [i | i <- [1..half], m `mod` i == 0]
  where
    half' :: Int -> Int
    half' 1 = 1
    half' 2 = 1
    half' 3 = 2
    half' x = x `div` 2 + 1
    half = half' m
