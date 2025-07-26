module Acronym (abbreviate) where

import Data.Char (toUpper, isAlpha, isLower, isUpper, isPunctuation)

splitCamelCase :: String -> String
hyphenToSpace :: String -> String
removePunctuation :: String -> String
toWordList :: String -> [String]

hyphenToSpace = _hyphenToSpace ""
  where
    _hyphenToSpace :: String -> String -> String
    _hyphenToSpace processed rest = case rest of
      "" -> processed
      c:cs -> 
        let newC = if c == '-' then ' ' else c in
          _hyphenToSpace (processed ++ [newC]) cs

splitCamelCase "" = ""
splitCamelCase [c] = [c]
splitCamelCase (c1:c2:rest) = _splitCamelCase "" c1 c2 rest
  where
    _splitCamelCase :: String -> Char -> Char -> String -> String
    _splitCamelCase processed nextChar followingChar ""
      | isAlpha nextChar && isAlpha followingChar && isLower nextChar && isUpper followingChar
        = processed ++ [nextChar, ' ', followingChar]
      | otherwise = processed ++ [nextChar, followingChar]
    _splitCamelCase processed nextChar followingChar (x:xs)
      | isAlpha nextChar && isAlpha followingChar && isLower nextChar && isUpper followingChar
        = _splitCamelCase (processed ++ [nextChar, ' ']) followingChar x xs
      | otherwise = _splitCamelCase (processed ++ [nextChar]) followingChar x xs

removePunctuation = filter (not . isPunctuation)

toWordList = _toWordList [] ""
  where
    _toWordList :: [String] -> String -> String -> [String]
    -- _WordList currentWordList currentWord rest
    _toWordList currentWordList "" "" = currentWordList
    _toWordList currentWordList currentWord "" = currentWordList ++ [currentWord]
    _toWordList currentWordList "" (x:xs) = case x of
      ' ' -> _toWordList currentWordList "" xs
      _ -> _toWordList currentWordList [x] xs
    _toWordList currentWordList currentWord (x:xs) = case x of
      ' ' -> _toWordList (currentWordList ++ [currentWord]) "" xs
      _ -> _toWordList currentWordList (currentWord ++ [x]) xs

abbreviate :: String -> String
abbreviate xs = map (toUpper . first) wordList
  where
    first (y:ys) = y
    wordList = toWordList $ removePunctuation $ hyphenToSpace $ splitCamelCase xs
