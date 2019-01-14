fibonacci :: Int -> Int
fibonacci n
   | n == 0 = 1
   | n == 1 = 1
   | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

{-- don't know about the signature --}
fibonacciTail =
    0 : 1 : zipWith (+) fibonacciTail (tail fibonacciTail)

proEuler2 =
    sum $ filter even [x | x <- takeWhile (<= 4000000) fibonacciTail]
