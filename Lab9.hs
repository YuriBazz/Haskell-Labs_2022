{-# LANGUAGE MultiWayIf #-}
import System.IO
import Data.List

--задание 1
myComp x y
	|x < y = GT
	|x > y = LT
	|otherwise = EQ

numInFileSort = do
	lst <- words <$> readFile "input.txt"
	let 
		lst1 = map read lst :: [Int]
		lst2 = filter (>= 0) lst1
		lst3 = sortBy myComp lst2
		lst4 = map show lst3
		rez = unwords lst4
	writeFile "output.txt" rez
		
--задание 2
myComp2 x y
	|snd x < snd y = GT
	|snd x > snd y = LT
	|otherwise = EQ

howManySym = do
	lst <- readFile "input.txt"
	let 
		lst1 = group $ sort $ lst ++ [' ' .. '~']
		lst2 = map (\x -> (head x, length x-1 )) lst1
		lst3 = sortBy myComp2 lst2 
		lst4 = map show lst3
		rez  = unlines lst4
	writeFile "output.txt" rez

--задание 3
--Использую слова да, больше, меньше
ugadai = help 1 1000000
help a b = do 
	putStrLn $ "Вы загадали число " ++ show c ++ " ?:"
	str <- getLine
	if 
		|str == "Да" -> do putStrLn $ "Ура! Я угадал!"
		|str == "Больше" -> do help c b
		|str == "Меньше" -> do help a c
		|otherwise -> do putStrLn "повторите ввод" 
		                 help a b
	where c = (a+b) `div` 2