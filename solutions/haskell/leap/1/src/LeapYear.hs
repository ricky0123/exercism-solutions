module LeapYear (isLeapYear) where

divisibleBy :: Integer -> Integer -> Bool
divisibleBy n k = n `mod` k == 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | div4 && (not div100 || div400) = True
  | otherwise = False
  where
    div4 = year `divisibleBy` 4
    div100 = year `divisibleBy` 100
    div400 = year `divisibleBy` 400
