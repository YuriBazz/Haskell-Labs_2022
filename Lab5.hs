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
    deriving (Show, Eq, Ord)
 data Hero = Hero {name :: String , xp :: Int , traits :: [(AttributeName, Int)]}
    deriving (Show, Ord, Eq)

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
 mightyTeam n heroes = 
   let 
      l = sortBy (flip compare) $ map (\hero -> (xp hero, hero)) heroes
      listScores = map fst l
      listHeroes = map snd l
   in 
      if sum listScores < n 
         then Nothing
         else 
            Just (take (length $ takeWhile (\(s, scores) -> s < n && not (null scores)) $
            iterate (\(s, scores) -> (s + ((!! 0) scores), (tail scores))) (0, listScores)) listHeroes)

-- д) 

 gatherTeam :: [(AttributeName,Int)] -> [Hero] -> Maybe [Hero]
 gatherTeam attributes heroes
       |null $ fst pairAttributesHeroes = Just $ snd pairAttributesHeroes
       |otherwise = Nothing
       where 
         pairAttributesHeroes = gatherTeam' attributes heroes 

 gatherTeam' attributes heroes = foldl (\res hero -> foldl (\res@(attrs, gathered) trait -> 
   let 
      attribute = filter ((== (fst trait)) . fst) attrs
      otherAttributes = filter ((/= (fst trait)) . fst) attrs 
   in
      if null attribute || (snd trait) < (snd $ head attribute) 
         then res
         else 
            if elem hero gathered 
               then (otherAttributes, gathered)
               else (otherAttributes, hero : gathered)) res (traits hero)) (attributes, []) heroes  
 
 -- е) 

 solidTeam :: [(AttributeName,Int)] -> [Hero] -> Maybe [Hero]
 solidTeam attributes heroes
       |null $ fst pairAttributesHeroes = Just $ optimiseTeam attributes (snd pairAttributesHeroes)
       |otherwise = Nothing
       where 
          pairAttributesHeroes = gatherTeam' attributes heroes 

 optimiseTeam :: [(AttributeName,Int)] -> [Hero] -> [Hero]
 optimiseTeam attributes heroes =
   let 
      vectorsHeroes = map (\hero@(Hero _ _ traits) -> (help attributes traits, hero)) heroes 
      optimisedHeroes = foldl (\vectorsHeroes vectorHero -> 
         let 
            newVectorsHeroes = (delete vectorHero vectorsHeroes)
            res = foldl1 (zipWith (+)) $ map fst $ newVectorsHeroes 
         in
            if elem 0 res
               then vectorsHeroes
               else newVectorsHeroes) vectorsHeroes vectorsHeroes
   in 
      map snd $ optimisedHeroes


 help :: [(AttributeName,Int)] -> [(AttributeName,Int)] -> [Int]
 help attributes traits = 
   foldl (\v attribute -> 
      let 
         requiredAttribute = filter ((== (fst attribute)) . fst) traits 
      in 
         if null requiredAttribute 
            then 0 : v
            else 
               if (snd attribute) > (snd $ head requiredAttribute) 
               then 0 : v
               else 1 : v) [] attributes
