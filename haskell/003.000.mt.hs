getMultiplesFilter :: Int -> [Int] -> [Int]
getMultiplesFilter div ls =
    filter (\x -> x `mod` div == 0) ls

getNotMultiplesFilter :: Int -> [Int] -> [Int]
getNotMultiplesFilter div ls =
    filter (\x -> x `mod` div /= 0) ls

eratostene :: [Int] -> [Int]
eratostene [] = []
eratostene (l:ls) =
    l : (eratostene (getNotMultiplesFilter l ls))

proEuler3 =
    maximum (takeWhile (< (floor . sqrt $ 600851475143)) (eratostene [2..]))
