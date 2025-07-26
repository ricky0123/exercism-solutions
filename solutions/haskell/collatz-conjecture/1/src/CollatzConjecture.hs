module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatzStep n 0
  where
    collatzStep :: Integer -> Integer -> Maybe Integer
    collatzStep 1 count = Just count
    collatzStep n _ | n < 1 = Nothing
    collatzStep n count | even n = collatzStep (n `div` 2) (count + 1)
    collatzStep n count | odd n = collatzStep (3 * n + 1) (count + 1)
    
