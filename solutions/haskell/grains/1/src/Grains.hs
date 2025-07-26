module Grains (square, total) where

import Data.Maybe (fromMaybe)
import Control.Applicative (liftA2)

square :: Integer -> Maybe Integer
square n
    | 1 <= n && n <= 64 = Just $ 2 ^ (n - 1)
    | otherwise = Nothing

total :: Integer
total = fromMaybe 0 $ foldl (liftA2 (+)) (Just 0) [square i| i <- [1..64]]