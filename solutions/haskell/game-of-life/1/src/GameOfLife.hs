module GameOfLife (tick) where

import Data.Maybe (fromMaybe)

(!?) :: [a] -> Int -> Maybe a
(!?) xs i
  | i < 0 || i > (length xs - 1) = Nothing
  | otherwise = Just $ xs !! i

tick :: [[Int]] -> [[Int]]
tick b = [ [ tickValue (Position r c b) | c <- [0..(length (b !! r) - 1)] ] | r <- [0..(length b - 1)] ]

data Position = Position { row :: Int, col :: Int, board :: [[Int]] }

value :: Position -> Maybe Int
value p = do
  r <- (board p) !? (row p)
  r !? (col p)

tickValue :: Position -> Int
tickValue p = fromMaybe 0 $ do
  oldValue <- value p
  let
    r = row p
    c = col p
    b = board p

    neighbors :: [Position]
    neighbors = [ Position (r-1) (c-1) b -- top left
                , Position (r-1) (c  ) b -- above
                , Position (r-1) (c+1) b -- top right
                , Position (r  ) (c-1) b -- left
                , Position (r  ) (c+1) b -- right
                , Position (r+1) (c-1) b -- bottom left
                , Position (r+1) (c  ) b -- bottom
                , Position (r+1) (c+1) b -- bottom right
                ]
    aliveNeighborCount = length [ p | p <- neighbors, (value p) == Just 1 ]
    isAlive = (oldValue == 1 && aliveNeighborCount == 2) || (aliveNeighborCount == 3)
  pure $ if isAlive then 1 else 0

