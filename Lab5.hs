-- ВНИМАНИЕ!!!! НЕ БЕРИТЕ ЭТУ ЛАБУ, ПОКА НЕ ИСЧЕЗНЕТ ЭТА НАДПИСЬ. ПОСЛЕДНИЕ ЗАДАЧИ РАБОТАЮТ НА ДОБРОМ СЛОВЕ, ТАК ЧТО ЖДЕМ ПРОВЕРКИ!!!!!!!
module Lab5 where
 import Data.List
 import Data.Maybe
 x1 = Var "x1"
 x2 = Var "x2"
 x3 = Var "x3"
 -- Задача 1

 data Prop = And Prop Prop | Or Prop Prop | Not Prop | Var String
    deriving Show

 -- a) 

 vars :: Prop -> [String] 
 vars (Var s) = [s]
 vars (And a b) = vars a `union` vars b
 vars (Or a b) = vars a `union` vars b
 vars (Not a) = vars a 

 -- б)

 truthValue :: Prop -> [(String, Bool)] -> Bool
 truthValue (And a b) lst = truthValue a lst && truthValue b lst
 truthValue(Or a b) lst = truthValue a lst || truthValue b lst
 truthValue (Not a) lst = not $ truthValue a lst
 truthValue (Var a) lst = head $ map snd $ filter (\(x,_) -> x == a ) lst

 -- в)

 tautology :: Prop -> Bool
 tautology prop = 
    let 
        helper :: [String] -> [Bool] -> [[(String, Bool)]]
        helper [] _ = [[]]
        helper l1 l2 = concat [(p:) `map` helper (tail l1) l2 | p <- [(head l1, y) | y <- l2]]
    in
        all (truthValue prop) $ helper (vars prop) [True, False]

-- Задача 2

 data AttributeName = Str | Dex | Wis | Acrobatics | Berserk | SpellCraft
    deriving (Show, Eq)
 data Hero = Hero {name :: String , xp :: Int , traits :: [(AttributeName, Int)]}
    deriving Show

 hero1 = Hero "Ornshtain" 100 [(Str, 15), (Dex, 20), (Wis, 10), (Acrobatics, 25), (Berserk, 20), (SpellCraft, 0)]
 hero2 = Hero "Smought" 100 [(Str, 20), (Dex, 15), (Wis, 10), (Acrobatics, 10), (Berserk, 30), (SpellCraft, 0)]
 hero3 = Hero "AshenOne" 1 [(Str, 0), (Dex, 0), (Wis, 0), (Acrobatics, 0), (Berserk, 0), (SpellCraft, 0)]
 hero4 = Hero "NewPlayer" 1 []

-- a) 

 findHero :: String -> [Hero] -> Maybe Hero
 findHero n list = find (\x -> name x == n) list

 -- б) Будет добавлять экспу

 addXp :: Int -> Hero -> Hero 
 addXp n hero = Hero (name hero) (n + xp hero) (traits hero)
 
 -- в) 

 incAttrLevel :: AttributeName -> Hero -> Hero 
 incAttrLevel atr hero = 
    Hero (name hero) (xp hero) (if x == Nothing then y ++ [(atr, 1)] else [(atr, x' + 1)] ++ delete (atr, x') y )
        where 
            y = traits hero
            x = lookup atr y
            x' = fromJust x

-- г)
 
 mightyTeam :: Int -> [Hero] -> Maybe [Hero]
 mightyTeam n lst = 
   let 
      myCompare a b =
         if xp a == xp b then EQ
         else 
            if xp a < xp b 
               then GT
               else LT
      mySort = sortBy (\x y -> myCompare x y)
   in
      find (\x-> n <= sum (map xp x))  (tail $ subsequences $ mySort lst)
      

-- д) 

 gatherTeam :: [(AttributeName, Int)] -> [Hero] -> Maybe [Hero]
 gatherTeam atrList heroList = 
   let
      f (atr, n) list = any (\x -> isJust (lookup atr (traits x)) && fromJust (lookup atr (traits x)) >= n ) list
      iter atrList heroList 
         | null atrList = True
         | otherwise = f (head atrList) heroList && iter (tail atrList) heroList
   in
      if iter atrList heroList
         then Just heroList
         else Nothing
 
 -- е) 

 solidTeam :: [(AttributeName,Int)] -> [Hero] -> Maybe [Hero]
 solidTeam atrList heroList =
   let
      x = sortBy (\x y -> compare (length x) (length y)) $ subsequences heroList
   in
      find (\x -> isJust $ gatherTeam atrList x) x
