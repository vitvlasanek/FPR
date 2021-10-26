faktorial :: Int -> Int
faktorial 0 = 1
faktorial n = n * faktorial (n-1)

combinations :: Int -> Int -> Int
combinations n k = faktorial n `div` (faktorial k * faktorial (n-k))

