length' :: [a] -> Int
length' []  = 0
length' (_:xs) = 1 + length' xs

sumIt :: [Int] -> Int
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs

getHead :: [a] -> a
getHead (x:_) = x

getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs

getLast' :: [a] -> a
getLast' (x:xs) | length xs == 0 = x
                | otherwise = getLast' xs