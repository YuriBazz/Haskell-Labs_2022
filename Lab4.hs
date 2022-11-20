module Lab4 where
 import Data.List

 -- Задача 1

 excludeDivisor :: Integer -> Integer -> Integer
 excludeDivisor n k = head $ dropWhile (\x -> mod x k == 0) $ iterate (`div` k) n

-- Задача 2 
 
 recSum :: Integer -> Integer -> [Integer]
 recSum t1 n = 
    let 
        mod10 = (`mod`  10)
        div10 = (`div` 10)
        num2lst :: Integer -> [Integer]
        num2lst x =
            let 
                iter x r
                    | x == 0 = r
                    | otherwise = iter (div10 x) ([fromIntegral (mod10 x)] ++ r)           
            in
                if x == 0 then [0] else iter x []
        s x = sum $ num2lst x
        f (k,tk) = (k + 1, tk + s (tk + k + 1)) 
    in
        map snd $ take (fromIntegral n) $ iterate f (1,t1)

-- Задача 3 

 newton :: Int -> [Integer]
 newton 0 = [1]
 newton n = 
    let 
        f (l, x) = if [] == tail l then ([], 0) else (tail l, head l + head (tail l))
    in 
        ( takeWhile (/= 0) $ map snd $ iterate f (newton (n - 1), 1) ) ++ [1]

-- Задача  4

 root :: (Double -> Double) -> Double -> Double -> Double -> Double
 root f a b eps = 
    let 
        g (a, b) = 
            if abs (b - x) < eps then (x,x) else
                if (f a) * (f x) < 0
                    then (a,x)
                    else
                        if (f x) * (f b) < 0 
                            then (x, b)
                            else (x, x)
            where x = (a + b) / 2
    in
      fst $ head $ dropWhile (\(x,y) -> x /= y) $ iterate g (a,b)

-- Задача 5

 sumSerie :: (Int->Double) -> Double -> Double
 sumSerie a eps = 
    let f (n, x) = (n + 1, (a (n + 1)) * (-1)^ (n + 1) )
    in  sum $ takeWhile (\x -> abs x > eps) $ map snd $ iterate f (1, (-1) * (a 1))

-- Задача 6

 allDigits :: Integer -> [Int]
 allDigits n =
    let 
        f (n,m) = if n == 0 then (0,0) else (div n 10, mod n 10)
        g n = map snd $ takeWhile (\(x,y) -> x /= y || y /= 0) $ tail $ iterate f (n, 101)
        h (n,l) = (n + 1, g (n + 1) ++ l)
    in
        reverse $ snd $ last $ take (fromIntegral n) $ iterate h (1, [1])
        
        
