faktorial :: Int -> Int
faktorial 1 = 1
faktorial n = n * faktorial (n-1)

fib :: Int->Int 
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


fib' :: Int -> Int 
fib' n = tmp n 1 1 where
    tmp 0 a _ = a
    tmp n a b = tmp (n-1) b (a+b)