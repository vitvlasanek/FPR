import qualified Data.Map as Mapa

dictionary = [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

toText :: String->[(Char, String)]->String
toText n l = krok n l []  where 
--krok :: String -> [(Char, String)] -> [Char] -> String
    --krok [] _ _ = []
    krok (x:xs) ((y,z):rest) m  | m == [] = krok xs l [x]
                                | m == z = y : krok (x:xs) rest []
                                | rest == [] = krok xs l (m ++ [x])
                                | xs == [] = []
                                | otherwise = krok (x:xs) rest m
