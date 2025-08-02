module Triangle (rows) where

import Data.Maybe (fromMaybe)
import Data.List (unfoldr)

(!?) :: [a] -> Int -> Maybe a
(!?) as i
  | i < 0 || i > length as - 1 = Nothing
  | otherwise = Just $ as !! i

rows :: Int -> [[Integer]]
rows x = take x $ [[1]] <> unfoldr step initial
  where
    initial = ([1], 2)
    step (lastRow, currentRowSize) =
      let
        newRow = [rowEntry lastRow i | i <- [0..(currentRowSize-1)]]
      in
        Just (newRow, (newRow, currentRowSize+1))

rowEntry :: [Integer] -> Int -> Integer
rowEntry prevRow i = a + b
  where
    a = fromMaybe 0 $ prevRow !? (i - 1)
    b = fromMaybe 0 $ prevRow !? i


