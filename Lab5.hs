module Lab5 where
 import Data.List
 -- Задача 1
 data Prop = And Prop Prop | Or Prop Prop | Not Prop | Var String
    deriving Show
 -- a) 
 vars :: Prop -> [String] 
 vars = nub . var 
 var :: Prop -> [String]
 var (Var s) = [s]
 var (And a b) = var a ++ var b
 var (Or a b) = var a ++ var b
 var (Not a) = var a 

 x1 = Var "x1"
 x2 = Var "x2"
 x3 = Var "x3"
 -- б)

 truthValue :: Prop -> [(String, Bool)] -> Bool
 truthValue (And a b) lst = truthValue a lst && truthValue b lst
 truthValue(Or a b) lst = truthValue a lst || truthValue b lst
 truthValue (Not a) lst = not $ truthValue a lst
 truthValue (Var a) lst = head $ map snd $ filter (\(x,_) -> x == a ) lst