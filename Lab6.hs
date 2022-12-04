module Lab6 where
-- ЕБАЛ Я В РОТ ЭТУ ЛАБУ
 import Data.List
 import Data.Maybe

 -- 1)
 class Monoid' a where
    (<>) :: a -> a -> a
    neutral :: a
    
    {-# MINIMAL (<>), neutral #-}
 instance Monoid' [t] where
    (<>) = (++)
    neutral = []
    

-- 2) 
 
 data Permutation = Permutation (Int , [(Int, Int)]) | NeutralPermutaion 
 -- Хранение перестановок: [(1,2),(2,3),(3,1)] для перестановки (3 1 2) == (1 2 3) -> Permutation (3, [(1,2),(2,3),(3,1)])
 -- [(Что переходит, Куда переходит)]
   deriving Show
 instance Monoid' Permutation where 
   neutral = NeutralPermutaion
   x <> NeutralPermutaion =  x
   NeutralPermutaion <> x = x
   Permutation (x, l1) <> Permutation (y, l2) = 
      if x == y 
         then Permutation (x, f l1 l2)
         else undefined
         where f a b = map (\ (x, y) -> (x, fromJust $ lookup y b)) a 

-- 3)  <> <- операция в моноиде 

 class Monoid' a => Group a where 
  opposite :: a -> a
  {-# MINIMAL opposite #-}

 instance Monoid' Double where
   (<>) = (+)
   neutral = 0.0
 instance Group Double where 
   opposite = ((-1) *)

-- 4) 
-- конечно же имелось ввиду не LinerSpace, а LinearSpace. Но постфактум переделывать очень грустно

 data Vec = Vec (Int, [Double]) | NeutralVec
   deriving Show
 class Group a => LinerSpace a where 
   scal :: Double -> a -> a
   {-# MINIMAL scal #-}
 
 instance Monoid' Vec where
   neutral = NeutralVec
   NeutralVec <> x = x
   x <> NeutralVec = x
   Vec (x, l1) <> Vec (y, l2) = 
      if x == y 
         then Vec (x, zipWith (+) l1 l2)
         else undefined

 instance Group Vec where        
   opposite (Vec (x , l)) = Vec (x , map (* (-1)) l)
   opposite NeutralVec    = NeutralVec

 instance LinerSpace Vec where
   scal 0 _ = NeutralVec
   scal _ NeutralVec = NeutralVec
   scal a (Vec (x, l)) = Vec (x, map (* a) l)

 
 
 
 
 instance Monoid' b => Monoid' (a -> b) where
   neutral = \_-> neutral
   (<>) f g = \x -> f x <> g x

 instance Group b => Group (a -> b) where
   opposite f = \ x -> opposite $ f x
 
 instance LinerSpace b => LinerSpace (a -> b) where
  scal t f = \x -> scal t (f x)

-- 5) 

 class LinerSpace a => HilbertSpace a where 
   (%) :: a -> a -> Double
 instance HilbertSpace Vec where
   Vec (x, l1) % Vec (y, l2) = 
      if x == y 
         then sum $ zipWith (*) l1 l2
         else undefined
   _ % NeutralVec = undefined
   NeutralVec % _ = undefined
   
 instance LinerSpace Double where 
  scal a x = a * x
 instance HilbertSpace (Double -> Double) where
  f % g = 
    let 
      delta = 1 / 10000
    in 
      delta * (sum $ map (\x -> f x * g x) [0,delta..1])
