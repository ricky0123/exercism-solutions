module Clock (addDelta, fromHourMin, toString) where



data Clock = Clock { hours :: Int, minutes :: Int }
  deriving Eq



fromHourMin :: Int -> Int -> Clock
fromHourMin hours' min' = addDelta hours' min' $ Clock 0 0

toString :: Clock -> String
toString clock = toString' $ fromHourMin (hours clock) (minutes clock)

toString' :: Clock -> String
toString' clock = fmt (hours clock) <> ":" <> fmt (minutes clock)

fmt :: Int -> String
fmt i
    | i < 10 = "0" <> show i
    | otherwise = show i

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour' min' clock = Clock hour'' min''
    where
        totalMinutes = (hour' + hours clock) * 60 + min' + minutes clock
        trueTotalMinutes = totalMinutes `mod` (24 * 60)
        (hour'', min'') = trueTotalMinutes `divMod` 60
