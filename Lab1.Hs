module Lab1 where

fact :: Int -> Int
fact n = if n == 0 then 1 else n * fact (n - 1)

-- Здача 4

reverseNumber :: Integer -> Integer
reverseHelper :: Integer -> Integer -> Integer
reverseHelper cur res 
     | cur == 0 = res
     | otherwise = reverseHelper (div cur 10) (10 * res + (mod cur 10))  

reverseNumber n = reverseHelper n 0

-- Задача 5
roots :: Double -> Double -> Double -> Double
maxRoot :: Int -> Int -> Int -> Double
roots a b c = 
    let   
        d = sqrt (b ^ 2 - 4 * a * c) 
        x1 = (-b + d) / (2 * a)
        x2 = (-b - d) / (2 * a)
    in max x1 x2

maxRoot a b c = roots (fromIntegral a ) (fromIntegral b ) (fromIntegral c)

-- Задача 6 
root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = let x = (a + b) / 2 in  
     if abs(b - x) < eps
        then x
        else 
            if (f a) * (f x) < 0
                then root f a x eps
                else 
                    if (f x) * (f b) < 0
                        then root f x b eps 
                        else x