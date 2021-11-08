import qualified Data.Map as Mapa

dictionary = [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

toText :: String->[(Char, String)]->String
--krok :: String -> String -> String
toText n y = krok n []  where 
    krok :: String -> [Char] -> String
    krok [] _ = []
    krok (x:xs) [] = krok xs x
    krok (x:xs) z   | Mapa.member z (Mapa.fromList y) =  (Mapa.fromList y Mapa.! z) ++ krok xs []
                    | Mapa.notMember z (Mapa.fromList y) = krok xs (z:x)