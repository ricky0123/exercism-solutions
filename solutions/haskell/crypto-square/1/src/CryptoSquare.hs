module CryptoSquare (encode) where

import Data.Char (toLower)

encode :: String -> String
encode xs = formatSquareCode standardizedInput rows columns
  where
    standardizedInput = standardize xs
    (rows, columns) = getRowsAndColumns (length standardizedInput)

formatSquareCode :: String -> Int -> Int -> String
formatSquareCode xs rows columns = unwords columnStrings
  where
    remainder = rows * columns - length xs
    newXs = xs <> replicate remainder ' '
    
    getChunks :: [String] -> Int -> String -> [String]
    getChunks acc chunkSize input =
      let
        (chunk, rest) = splitAt chunkSize input
        newAcc = acc <> [chunk]
      in
        case rest of
          [] -> newAcc
          _ -> getChunks newAcc chunkSize rest
    
    square = getChunks [] columns newXs
    columnStrings = [concatMap (\ys -> [safeIndex ys i ' ']) square | i <- [0..columns-1]]

safeIndex :: [a] -> Int -> a -> a
safeIndex xs i def
  | i < 0         = def
  | otherwise     = go xs i
  where
    go [] _      = def
    go (y:ys) 0  = y
    go (_:ys) n  = go ys (n - 1)

standardize :: String -> String
standardize xs = map toLower $ filter (`notElem` punctuation) xs
  where
    punctuation :: String
    punctuation = ",.?;:!@% "

getRowsAndColumns :: Int -> (Int, Int)
getRowsAndColumns messageLength = head $ filter (\(r, c) -> r * c >= messageLength) candidates
  where
    root :: Int
    root = round (sqrt (fromIntegral messageLength) :: Double)

    candidates :: [(Int, Int)]
    candidates
      = [ (root -1, root - 1)
        , (root-1, root )
        , (root, root)
        , (root, root +1)
        , (root+1, root+1)
        , (root+1, root + 2)
        ]
