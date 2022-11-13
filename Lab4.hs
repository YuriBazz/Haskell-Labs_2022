module Lab4 where
 import Data.List

 -- Задача 1

 excludeDivisor :: Integer -> Integer -> Integer
 excludeDivisor n k = 
    let 
        helper1 n k l = fst (break (\x -> n + 1 <= k ^ x ) l) -- дает список стпеней р таких, что k^p <= n 
        listOfP = helper1 n k [0..]
        listOfK = map (k ^ ) listOfP
    in
      head (snd (break (\q -> (any (\y -> n == q * y ) listOfK) && mod q k /= 0) [1..n]))

-- Задача 2 
 
 recSum :: Integer -> Integer -> [Integer]
 recSum t1 n 