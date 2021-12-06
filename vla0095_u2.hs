import Data.List (sort, nub, init, tail, take, drop)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Show)
data Rank = Numeric Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card = Card Rank Suit deriving (Eq, Show)
type Hand = [Card]
data Category = RoyalFlush
              | StraightFlush
              | Four
              | FullHouse
              | Flush
              | Straight
              | Three
              | TwoPair
              | Pair
              | HighCard deriving (Eq, Show)

--převedení karet na Int
valueOf :: Rank -> Int
valueOf Ace = 14
valueOf King = 13
valueOf Queen = 12
valueOf Jack = 11
valueOf (Numeric x) = x

--seřazení karet sestupně
mkList :: Hand -> [Int]
mkList [] = []
mkList (Card r _ : cs) = mkList cs ++ [valueOf r]

--Royal Flush – královská postupka (10, J, Q, K, A v jedné barvě)
    --pomocí Straight + head == 14 + barvy
isRf :: [Int] -> Bool
isRf [] = False 
isRf (x:xs) | isSt (x:xs) && x == 14= True
            | otherwise = False

--Straight flush – čistá postupka (pět karet v řadě a ve stejné barvě)
    --pomocí Straight + barvy

--Four of a kind – čtveřice stejné hodnoty (někdy také „Poker“)
isFo :: [Int] -> Bool
isFo x  | length(nub (init x)) == 1 = True
        | length(nub (tail x)) == 1 = True
        | otherwise = False 

--Full house – trojice a dvojice stejných hodnot
isFh :: [Int] -> Bool
isFh x  | length (nub x) <= 2 = True 
        | otherwise = False 

--Flush – barva (pět karet stejné barvy)
flush :: Hand -> Bool
flush [x] = True 
flush [] = False  
flush (Card r1 s1 : Card r2 s2 : cs)    | s1 == s2 = flush (Card r2 s2 : cs)
                                        | otherwise = False

--Straight – postupka (pět karet v řadě v různých barvách)
isSt :: [Int] -> Bool
isSt [x] = True
isSt [] = True 
isSt (x:y:rest) | x == (y+1) = isSt (y:rest)
                | x == 14 && y == 5 = isSt (y:rest)
                | otherwise = False

isTh :: [Int] -> Bool
isTh x  | length(nub (take 3 x)) == 1 = True
        | length(nub (drop 2 x)) == 1 = True
        | otherwise = False 

--Two pair – dva páry
isTp :: [Int] -> Bool
isTp x  | length (nub x) <= 3 = True 
        | otherwise = False 

--One pair – jeden pár
isPa :: [Int] -> Bool
isPa x  | length (nub x) <= 4 = True 
        | otherwise = False 

--High card – vysoká karta (karta nejvyšší hodnoty)
    --v decide.otherwise

--vyhodnocovací pořadí + ošetření High card
decide :: Hand -> Category 
decide x    | isRf (mkList x) && flush x = RoyalFlush
            | isSt (mkList x) && flush x = StraightFlush
            | isFo (mkList x) = Four
            | isFh (mkList x) = FullHouse
            | flush x = Flush
            | isSt (mkList x) = Straight
            | isTh (mkList x) = Three
            | isTp (mkList x) = TwoPair
            | isPa (mkList x) = Pair
            | otherwise = HighCard

-- Prelude> decide [Card (Numeric 2) Hearts,Card (Numeric 2) Clubs,Card Ace Hearts,Card Ace Clubs,Card King Spades]
-- TwoPair
-- Prelude> decide [Card (Numeric 2) Hearts,Card (Numeric 2) Clubs,Card Ace Hearts,Card Ace Clubs,Card Ace Spades]
-- FullHouse
-- Prelude> decide [Card Ace Hearts,Card (Numeric 2) Hearts,Card (Numeric 5) Hearts,Card (Numeric 3) Hearts,Card (Numeric 4) Clubs]
-- Straight
-- Prelude> decide [Card (Numeric 2) Hearts,Card (Numeric 5) Clubs,Card Ace Hearts,Card King Clubs,Card Jack Spades]
-- HighCard