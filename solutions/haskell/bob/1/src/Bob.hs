module Bob (responseFor) where
import Data.List (last)
import Data.Char (isUpper, isSpace, isAlpha)

responseFor :: String -> String
responseFor xs
  | isSilence = "Fine. Be that way!"
  | isNormalQuestion = "Sure."
  | isYell = "Whoa, chill out!"
  | isYellingQuestion = "Calm down, I know what I'm doing!"
  | otherwise = "Whatever."
  
  where
    isYell = hasText && lettersAreUpper && not endsInQuestionMark
    isNormalQuestion = (not hasText || not lettersAreUpper) && endsInQuestionMark
    isYellingQuestion = hasText && lettersAreUpper && endsInQuestionMark

    hasText = any isAlpha xs

    endsInQuestionMark = endsIn xsNonWhite '?'
    isSilence = all isSpace xs
    lettersAreUpper = all isUpper xsLetters
    
    xsLetters = filter isAlpha xs
    xsNonWhite = filter (not . isSpace) xs

endsIn :: String -> Char -> Bool
endsIn [] _ = False
endsIn str char = last str == char
