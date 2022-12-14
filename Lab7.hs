module Lab7 where
 import Data.Maybe
 import Data.Tuple
 import Data.List

 -- 1)
 dec :: [a] -> [b] -> [(a,b)]
 dec l1 l2 = foldl (\r x -> (map (\y -> (x , y)) l2) ++ r) [] l1

 dec' :: [a] -> [b] -> [(a,b)]
 dec' l1 l2 = (,) <$> l1 <*> l2

 --2) 
 addToList :: [Maybe Int] -> [Maybe Int]
 addToList l = map (\x -> Just ((+) 3 $ fromJust x)) l
 
 addToList' :: [Maybe Int] -> [Maybe Int]
 addToList' l = fmap (+3) <$> l

 -- 3) 
 newGrowth1 :: (String, Int) -> Int -> (String, Int)
 newGrowth1 (x, y) z = (x, y + z)

 newGrowth1' :: (String, Int) -> Int -> (String, Int)
 newGrowth1' x z =(+z) <$> x 

 newGrowth2 :: (Int, String) -> Int -> (Int, String)
 newGrowth2 (x, y) z = (x + z, y)

 newGrowth2' :: (Int, String) -> Int -> (Int, String)
 newGrowth2' x z = swap (newGrowth1' (swap x) z)

 -- 4)
 data Point = Point { x :: Double, y :: Double }
    deriving (Show, Ord, Eq)
 data Line = Line { a :: Double, b :: Double, c :: Double }
    deriving (Show, Ord, Eq)

 f1 :: Line -> Line -> Maybe Point
 f1 (Line a1 b1 c1) (Line a2 b2 c2) = 
    let 
        d1 = (-c1) * b2 - (-c2) * b1
        d2 = a1 * (-c2) - a2 * (-c1)
        d = a1 * b2 - a2 * b1
    in 
        if d == 0 
            then Nothing
            else 
                Just (Point (d1 / d) (d2 / d))

 f2 :: [Line] -> [Maybe Point]
 f2 l = (\(l1, l2) -> (f1 l1 l2)) <$> (dec' l l)

 decComp :: Point -> Point -> Ordering
 decComp (Point a1 b1) (Point a2 b2)
    |a1 > a2 = GT
    |a1 < a2 = LT
    |otherwise = if b1 - b2 > 0 then GT else if b1 - b2 < 0 then LT else EQ

 maxPoint :: [Line] -> Maybe Point
 maxPoint l = 
    let 
        l' = catMaybes $ filter (/= Nothing) $ f2 l
    in
        if null l'
            then Nothing
            else Just $ maximumBy decComp l'