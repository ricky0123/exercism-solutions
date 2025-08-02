module SecretHandshake (handshake) where

import Data.List (reverse)

handshake :: Int -> [String]
handshake n = 
    let
      (rest1, p1) = n `divMod` 2
      (rest2, p2) = rest1 `divMod` 2
      (rest3, p4) = rest2 `divMod` 2
      (rest4, p8) = rest3 `divMod` 2
      p16 = rest4 `mod` 2
      reverseFunc = if (p16 == 1) then reverse else id
    in
      reverseFunc $
        (if p1 == 1 then ["wink"] else [])
        <> (if p2 == 1 then ["double blink"] else [])
        <> (if p4 == 1 then ["close your eyes"] else [])
        <> (if p8 == 1 then ["jump"] else [])
