
module Lab2 where 
 fact :: Int -> Int
 fact n = if n == 0 then 1 else n * fact (n - 1)

 dFact :: Integer -> Integer 
 dFact n 
     | n == 1 = 1
     | n == 2 = 2
     | otherwise = n * dFact (n - 2)

 sumOfDigits :: Integer -> Int
 sumOfDigits 0 = 0
 sumOfDigits n = fromIntegral ((mod n 10) :: Integer) + sumOfDigits (div n 10) 

 powOf2 :: Integer -> Int
 powOf2 x = 
    let 
        iter :: Integer -> Int -> Int
        iter x r 
             | x == 1 = r 
             | x `mod` 2 == 0 = iter (x `div` 2) (r + 1)
             | otherwise = (-1)
    in iter x 0 
 
 qntPoints :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer 
 qntPoints x1 y1 r1 x2 y2 r2 = 
    let 
        dist = (x2 - x1) ^ 2 + (y2 - y1) ^ 2
        iter a1 b1 c1 a2 b2 c2 d
             | a1 == a2 && b1 == b2 && c1 == c2 = error "Окружности совпали"
             | (c1 - c2) ^ 2 == d = 1 -- внутреннее касание 
             | (c1 + c2) ^ 2 == d = 1 -- внешнее касание
             | (c1 - c2) ^ 2 > d || (c1 + c2) ^ 2 < d = 0
             | otherwise = 2
    in iter x1 y1 r1 x2 y2 r2 dist 

 smallerX :: Double -> Double
 mySin :: Double -> Double -> Double
 smallerX x 
     | x >= 0 && x < 2 * pi = x
     | otherwise = smallerX (x - 2 * pi)
 mySin x eps =
    let 
        iter :: Double -> Double -> Double -> Double 
        iter x eps k
             | abs a <= eps = 0
             | otherwise = a + iter x eps (k + 1) 
             where 
                a = ((-1) ^ round k) * (x ^ (1 + 2 *  round k))  / (fromIntegral (fact (1 + 2 * round k)))
                
    in
        iter (smallerX x) eps 0

 f1 :: Integer -> Integer -> Integer 
 f1 x y = x ^ y
 f1' = (^)
 f2 :: Integer -> Integer -> Integer 
 f2 x y = (x + 1) ^ y
 f2' = (^) . (+ 1)
 f3 :: Integer -> Integer -> Integer
 f3 x y = x ^ (y - 3)
 h3 = negate . (3 - )
 f3' = flip $ flip f1' . h3
 f4 :: Integer -> Integer -> Integer
 f4 x y = (x + 1) ^ (y - 3)
 f4' =  f3'. (+ 1)
