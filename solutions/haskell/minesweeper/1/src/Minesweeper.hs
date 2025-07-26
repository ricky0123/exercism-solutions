module Minesweeper where

(!?) :: [a] -> Int -> Maybe a
(!?) xs i
  | i < 0 || i > (length xs - 1) = Nothing
  | otherwise = Just $ xs !! i

annotate :: [String] -> [String]
annotate b = [ [ getNewValue p | p <- row ] | row <- (getPositions b) ]

data Position = Position { pos :: (Int, Int) , board :: [String] }

valueAtPosition :: Position -> Maybe Char
valueAtPosition p = do
  let
    (x, y) = pos p
  row <- board p !? x
  row !? y

getPositions :: [String] -> [[Position]]
getPositions board =
  [ [ Position (x, y) board | x <- [0..(length (board !! y) - 1)] ] | y <- [0..(length board - 1)] ]

getNewValue :: Position -> Char
getNewValue p
  | isMine = '*'
  | nMinesAdjacent == 0 = ' '
  | otherwise = intToDigit nMinesAdjacent
    where
      currentValue = valueAtPosition p
      isMine = currentValue == Just '*'
      nMinesAdjacent = numberOfSurroundingMines p

      intToDigit :: Int -> Char
      intToDigit i = (show i) !! 0

numberOfSurroundingMines :: Position -> Int
numberOfSurroundingMines p =
  let
    (x, y) = pos p
    b = board p
    surroundingPositions =
      [ Position (x-1, y-1) b -- upper left
      , Position (x  , y-1) b -- above
      , Position (x+1, y-1) b -- upper right
      , Position (x-1, y  ) b -- left
      , Position (x+1, y  ) b -- right
      , Position (x-1, y+1) b -- lower left
      , Position (x  , y+1) b -- below
      , Position (x+1, y+1) b -- lower right
      ]
    neighborValues = map valueAtPosition surroundingPositions
    mines = filter ((==) (Just '*')) neighborValues
  in
    length mines
