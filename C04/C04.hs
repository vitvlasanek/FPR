import Distribution.Simple.Utils (xargs)
import System.Directory.Internal.Prelude (toUpper)
take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x:y:z)    | x < y = minimum' (x:z)
                    | otherwise = minimum' (y:z)

divisors :: Int -> [Int]
divisors n = tmp n where
    tmp x   |x == 0 = []
            |mod n x == 0 = x : tmp (x-1)
            |otherwise = tmp (x-1)

zipThem :: [a] -> [b] -> [(a,b)]
zipThem (x:xs) (y:ys) = (x,y) : zipThem xs ys
zipThem _ _ = []

dotProduct :: [a] -> [b] -> [(a,b)]
dotProduct [] _ = []
dotProduct _ [] = []
dotProduct (x:xs) ys = tmp ys ++ dotProduct xs ys where
  tmp [] = []
  tmp (b:bs) = (x,b) : tmp bs

fibonacci :: Int -> Int
fibonacci n = fst (tmp n) where
    fibStep (a,b) = (b, a+b)
    tmp 0 = (0,1)
    tmp x = fibStep (tmp (x-1))

allToUpper :: String -> String
allToUpper = map toUpper