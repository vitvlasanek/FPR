gcd' :: Int -> Int -> Int 
gcd' a b | a == b = a
         | a<b = gcd' a (b-a)
         | otherwise = gcd' (a-b) b