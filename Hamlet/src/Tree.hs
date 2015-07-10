module Tree where

import Data.Function
import Data.List
import Data.Ord
import Debug.Trace


data Tree n = Node {node :: n, childNodes :: [Tree n]} deriving (Show)

{-
 -  f - function to replace Node
 -  g - function to replace Cons
 -  a - something to replace Nil
 -}
redtree :: (t -> t1 -> t2) -> (t2 -> t1 -> t1) -> t1 -> Tree t -> t2
redtree f g a Node {node = n, childNodes = c} = f n (redtree' f g a c)
redtree' f g a (hd : rest) = g (redtree f g a hd) (redtree' f g a rest)
redtree' _ _ a [] = a

maptree :: (t -> b) -> Tree t -> Tree b
maptree f = redtree (Node . f) (:) []

reptree :: (t -> [t]) -> t -> Tree t
reptree f a = Node a (map (reptree f) (f a))

maximize :: Ord t => Tree t -> t
maximize Node {node = n, childNodes = []} = n
maximize Node {node = _, childNodes = c} = maximum (map minimize c)
minimize :: Ord t => Tree t -> t
minimize Node {node = n, childNodes = []} = n
minimize Node {node = _, childNodes = c} = minimum (map maximize c)

prune :: (Num a, Eq a) => a -> Tree n -> Tree n
prune 0 Node {node = n, childNodes = _} = Node n []
prune r Node {node = n, childNodes = c} = Node n $ map (prune (r - 1)) c

-- alpha beta method
maximize' :: Ord a => Tree a -> [a]
maximize' Node {node = n, childNodes = []} = n : []
maximize' Node {node = _, childNodes = c} = mapmin (map minimize' c)
minimize' :: Ord a => Tree a -> [a]
minimize' Node {node = n, childNodes = []} = n : []
minimize' Node {node = _, childNodes = c} = mapmax (map maximize' c)

-- map min/max
mapmin :: Ord a => [[a]] -> [a]
mapmin (nums : rest) = (minimum nums) : (omitmin (minimum nums) rest)
mapmax :: Ord a => [[a]] -> [a]
mapmax (nums : rest) = (maximum nums) : (omitmax (maximum nums) rest)

omitmin :: Ord t => t -> [[t]] -> [t]
omitmin pot [] = []
omitmin pot (nums : rest)
    | minleq nums pot = omitmin pot rest
    | otherwise = (minimum nums) : (omitmin (minimum nums) rest)

minleq :: Ord a => [a] -> a -> Bool
minleq [] pot = False
minleq (num : rest) pot
    | num <= pot = True
    | otherwise = minleq rest pot

omitmax :: Ord t => t -> [[t]] -> [t]
omitmax pot [] = []
omitmax pot (nums : rest)
    | maxgeq nums pot = omitmax pot rest
    | otherwise = (maximum nums) : (omitmax (maximum nums) rest)

maxgeq :: Ord a => [a] -> a -> Bool
maxgeq [] pot = False
maxgeq (num : rest) pot
    | num >= pot = True
    | otherwise = maxgeq rest pot

highfirst :: Ord a => Tree a -> Tree a
highfirst Node {node = n, childNodes = c} = Node n (sortBy (comparing node) (map lowfirst c))
lowfirst :: Ord a => Tree a -> Tree a
lowfirst Node {node = n, childNodes = c} = Node n (sortBy (flip $ comparing node) (map highfirst c))
