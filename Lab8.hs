{-# LANGUAGE MultiWayIf, DatatypeContexts #-}

import Data.List
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

--задание 1
--пример ввода
-- runReader (findroot 1 4) (sin,0.00000001)
findroot :: Double -> Double -> Reader (Double -> Double , Double ) Double
findroot a b = do
	(f,eps) <- ask
	if
		| b-a < eps     	-> return point
		| f point * f a > 0 -> findroot point b
		| f point * f b > 0 -> findroot a point
	where
	point = (a+b)/2
--задание 2 
-- для того чтобы работало нужен {-# DatatypeContexts #-} 
data (Show a, Eq a, Ord a) => Tree a = Empty | Node {el :: a, left :: (Tree a), right :: (Tree a) }
	deriving (Show,Eq,Ord)
--деревья
tree1 :: Tree Int
tree1 = Node 1 Empty Empty
tree2 :: Tree Int
tree2 = Node 10 (Node 8 (Node 4 (Node 1 Empty Empty) Empty) Empty) (Node 12 Empty Empty)

treeEmpty :: Tree a -> Bool
treeEmpty Empty = True
treeEmpty _ = False

--для сообщения использую англ т.к. Русский не работает
--пример ввода
--runWriter (inTree tree2 1)
inTree :: Tree Int -> Int -> Writer String Bool
inTree tr x = do
	let 
		root = el tr
		leftTree = left tr
		rightTr = right tr
	if
		|treeEmpty tr -> do return False
		|x == root -> do return True
		|x < root -> do 
			tell ("Left tree. ")
			inTree leftTree x
		|x > root -> do 
			tell ("Right tree. ") 
			inTree rightTr x
			
--задание 3
--пример ввода
--split ",. " " Hello, world !.. "

help1 :: String -> String -> Int -> Writer String String
help1 str delim flag = do
	if	
		|null str -> do return str
		|any (== x) delim -> do help1 (tail str) delim 1
		|flag == 1 -> do return str
		|otherwise -> do 
			tell ([x])
			help1 (tail str) delim 0
	where
		x = head str

split :: String -> String -> [String]
split delim str =
	if
		|null newStr -> [newWord]
		|null newWord -> split delim newStr
		|otherwise -> concat [[newWord], split delim newStr]
	where
		(newStr,newWord) = runWriter (help1 str delim 0)

