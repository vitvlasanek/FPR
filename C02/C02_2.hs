leapYear :: Int -> Bool
leapYear x  | x `mod` 400 == 0 = True
            | x `mod` 100 == 0 = False
            | otherwise = x `mod` 4 == 0