module Lib
    ( Arbol(Vacio, Node),
    Walkable(walkToList, walkWithFunction),
    arbol1
    ) where

import System.Random


arbol1 :: IO (Arbol Int)
arbol1 = do
    g <- getStdGen
    n <- (randomRIO (1,100))
    return $ fromList $ take n (randomRs (1,6::Int) g)

class Walkable w where
    walkWithFunction :: (a -> b) -> w a -> w b
    walkToList :: w a -> [a]
    fromList :: [a] -> w a

data Arbol a = Vacio | Node a (Arbol a) (Arbol a) deriving Show

instance Walkable Arbol where
    walkWithFunction f Vacio = Vacio
    walkWithFunction f (Node e (left) (right)) = Node (f e) (walkWithFunction f left) (walkWithFunction f right)
    walkToList Vacio = []
    walkToList (Node e left right) = e:(walkToList left) ++ (walkToList right)
    fromList [] = Vacio
    fromList (x:xs) = Node x leftTree rightTree where
        splitted = splitList xs
        leftTree = fromList $ fst splitted
        rightTree = fromList $ snd splitted
        splitList :: [a] -> ([a], [a])
        splitList [] = ([], [])
        splitList [x] = ([x], [])
        splitList (x:y:xys) = (x:xs, y:ys) where (xs, ys) = splitList xys

-- TODO: Implement a data type similar to a list and make it an instance of Walkable and print walkWithFunction and walkToList from this data type in the main file
