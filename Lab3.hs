module Lab3 where
 import Data.List
 import Data.Tuple

-- Задача 1

 num2lst :: Integer -> [Int]
 lst2num :: [Int] -> Integer
 mod10 = (`mod`  10)
 div10 = (`div` 10)
 num2lst x =
    let 
        iter x r
             | x == 0 = r
             | otherwise = iter (div10 x) ([fromIntegral (mod10 x)] ++ r)           
    in
        if x == 0 then [0] else iter x []
 lstToNum l
     | l == [] = 0
     | otherwise = fromIntegral (head l) + (lstToNum $ tail l) * 10
 lst2num = lstToNum . reverse 

 -- Задача 2

 sumSquares1 :: Num a => [a] -> a
 sumSquares2 :: Num a => [a] -> a
 sumSquares3 :: Num a => [a] -> a
 up2 :: Num a => a -> a
 up2 = (^ 2)
 sumSquares1 [] = 0
 sumSquares1 l = up2 (head l) + sumSquares1 (tail l) 
 sumSquares2 l =
    let 
        iter [] r = r
        iter l' r = iter (tail l') (r + up2 (head l'))
    in
        iter l 0
 sumSquares3 = sum . (map up2)

-- Задача 3

 posMax :: [String] -> String
 posMax = map maximum

-- Задача 4

 minAngle :: [(Double, Double)] -> (Double, Double)
 pointToAngle x = if y >= 0 && y < 2* pi then y else 2 * pi + y where y = uncurry atan2 (swap x)
 decComp a b 
    | pointToAngle a > pointToAngle b = LT
    | pointToAngle a == pointToAngle b = if (fst a)^2 + (snd a)^2  > (fst b)^2 + (snd b)^2  then GT else LT 
    | otherwise = GT
 minAngle = maximumBy decComp

-- Задача 5

 upperHalf :: (Fractional a, Ord a) => [a] -> [a]
 median :: (Ord a, Fractional a) => [a] -> a
 median x =
    if odd n
      then sort x !! (n `div` 2)
      else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x
 upperHalf l = filter (> x) l where x = median l

-- Задача 6

 mostFrequent :: String -> (Char, String)
 freqOfEl x s = length $ elemIndices x s
 mostFrequent l = 
    let
        decComp x y 
            | freqOfEl x l < freqOfEl y l = LT
            | freqOfEl x l == freqOfEl y l = EQ
            | otherwise = GT
        x = maximumBy decComp l
    in
        (x, filter (\y -> x /= y) l)

