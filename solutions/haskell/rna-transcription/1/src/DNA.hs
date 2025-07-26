module DNA (toRNA) where

complement :: Char -> Either Char Char
complement 'G' = Right 'C'
complement 'C' = Right 'G'
complement 'T' = Right 'A'
complement 'A' = Right 'U'
complement c = Left c

toRNA :: String -> Either Char String
toRNA = toRNA' (Right "")
  where
    toRNA' (Right rna) "" = Right rna
    toRNA' (Right rna) (x:xs) = case complement x of
      Left c -> Left c
      Right c -> toRNA' (Right (rna ++ [c])) xs
