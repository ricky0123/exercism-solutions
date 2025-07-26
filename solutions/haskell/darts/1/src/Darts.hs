module Darts (score) where

type Radius = Float

getRadius :: Float -> Float -> Radius
getRadius x y = sqrt (x**2 + y**2)

score :: Float -> Float -> Int
score x y
  | rad <= 1 = 10
  | 1 < rad && rad <= 5 = 5
  | 5 < rad && rad <= 10 = 1
  | otherwise = 0
  where
    rad = getRadius x y
  
