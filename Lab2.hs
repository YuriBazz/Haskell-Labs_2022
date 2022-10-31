module Lab2 where 
 
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