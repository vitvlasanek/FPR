max2 :: Int -> Int -> Int
max2 x y | x >= y = x
         |otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = (x `max2` y) `max2` z