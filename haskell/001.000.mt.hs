getMultiples :: Int -> [Int] -> [Int]
getMultiples div ls =
    [x | x <- ls, x `mod` div == 0]

proEuler1 :: Int
proEuler1 =
    sum3 + sum5 - sum15
    where
        sum3 = sum (getMultiples 3 [1..1000])
        sum5 = sum (getMultiples 5 [1..1000])
        sum15 = sum (getMultiples 15 [1..1000])
