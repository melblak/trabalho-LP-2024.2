quantLinhas :: [Int] -> Int
quantLinhas [] = 0
quantLinhas (x:xs) 
    | x - head xs /= 0 = 1 + quantLinhas xs
    | otherwise = 0
