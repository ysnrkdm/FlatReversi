module Search (
    alphabeta,
    Result(..)
) where

-- friends
import qualified BitBoard
import qualified Move
import qualified MoveGenerator
import qualified Eval
-- GHC

-- libraries

-- std
import Data.Function
import Data.List
import Data.Ord
import Debug.Trace

data Result = Result {va :: Eval.Value, pv :: [Move.Mv]} deriving (Eq, Ord)
instance Show Result where
    show (Result va pv) = "va: " ++ (show va) ++ ", pv : " ++ (show pv)

data Tree n = Node {node :: n, childNodes :: [Tree n]} deriving (Show)

conv :: Move.Mv -> Result -> Result
conv mv res = Result (-va res) (mv : pv res)

minmax :: Int -> BitBoard.Bb -> Result
minmax 0 bd = Result (Eval.eval bd) []
minmax dep bd = maximumBy (compare `on` va) nexts
    where
        nexts = map next $ MoveGenerator.moveGenerationFull bd
        next mv = conv mv . minmax (dep - 1) $ BitBoard.move bd mv

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

moves :: (BitBoard.Bb, Result) -> [(BitBoard.Bb, Result)]
moves (bd, result) =
    map (\ x -> (BitBoard.move bd x, (conv x result))) $ MoveGenerator.moveGenerationFull bd

reptree :: (t -> [t]) -> t -> Tree t
reptree f a = Node a (map (reptree f) (f a))

gametree :: BitBoard.Bb -> Tree (BitBoard.Bb, Result)
gametree p = reptree moves $ (p, Result 0 [])

maximize :: Ord t => Tree t -> t
maximize Node {node = n, childNodes = []} = n
maximize Node {node = _, childNodes = c} = maximum (map minimize c)
minimize :: Ord t => Tree t -> t
minimize Node {node = n, childNodes = []} = n
minimize Node {node = _, childNodes = c} = minimum (map maximize c)

prune :: (Num a, Eq a) => a -> Tree n -> Tree n
prune 0 Node {node = n, childNodes = _} = Node n []
prune r Node {node = n, childNodes = c} = Node n $ map (prune (r - 1)) c

-- minmax method
evaluate :: BitBoard.Bb -> Eval.Value
evaluate = maximize . maptree (Eval.eval . fst) . (prune 3) . gametree

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

-- alphabeta
alphabeta :: (Num a, Eq a) => a -> BitBoard.Bb -> Result
alphabeta depth = normalizeResult . maximum . maximize' . maptree (\x -> ((Eval.eval . fst) x, snd x)) . (prune depth) . gametree

normalizeResult :: (Eval.Value, Result) -> Result
normalizeResult (value, result) =
    Result value pvs
    where
        pvs = if length pvFromRes > 0 then pvFromRes else [Move.Nil]
        pvFromRes = (reverse $ pv result)