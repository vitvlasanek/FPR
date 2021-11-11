import Data.Map (member, fromList, (!))
import Data.Tuple (swap)
import Data.List (map)

dictionary = [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

-- verze bez importovaných modulů
toText :: String->[(Char, String)]->String
toText num list = krok num list []  where 
    krok (n:ns) ((x,y):rest) mem    | mem == [] = krok ns list [n]
                                    | mem == y = x : krok (n:ns) rest []
                                    | rest == [] = krok ns list (mem ++ [n])
                                    | otherwise = krok (n:ns) rest mem
    krok [] ((x,y):rest) mem        | mem == y = [x]
                                    | otherwise = krok [] rest mem

-- verze s moduly
toText' :: String->[(Char, String)]->String
toText' n l = krok n []  where 
    krok [] z = [(fromList (map (swap) l) ! z)]
    krok (x:xs) z                   | member z (fromList (map (swap) l)) = (fromList (map (swap) l) ! z) : krok (x:xs) []
                                    | otherwise = krok xs (z ++ [x])
