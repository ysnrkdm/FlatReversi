module Search (
    alphabeta,
    Result(..)
) where

-- friends
import qualified BitBoard
import qualified Move
import qualified MoveGenerator
import qualified Eval
import qualified Tree
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

conv :: Move.Mv -> Result -> Result
conv mv res = Result (-va res) (mv : pv res)

--minmax :: Int -> BitBoard.Bb -> Result
--minmax 0 bd = Result (Eval.eval bd) []
--minmax dep bd = maximumBy (compare `on` va) nexts
--    where
--        nexts = map next $ MoveGenerator.moveGenerationFull bd
--        next mv = conv mv . minmax (dep - 1) $ BitBoard.move bd mv

moves :: (BitBoard.Bb, Result) -> [(BitBoard.Bb, Result)]
moves (bd, result) =
    map (\ x -> (BitBoard.move bd x, (conv x result))) $ MoveGenerator.moveGenerationFull bd

gametree :: BitBoard.Bb -> Tree.Tree (BitBoard.Bb, Result)
gametree p = Tree.reptree moves $ (p, Result 0 [])

--evaluate :: BitBoard.Bb -> Eval.Value
--evaluate = Tree.maximize . Tree.maptree (Eval.eval . fst) . (Tree.prune 3) . gametree

-- alphabeta
alphabeta :: (Num a, Eq a) => a -> BitBoard.Bb -> Result
alphabeta depth = normalizeResult . maximum . Tree.maximize' . Tree.maptree (\x -> ((Eval.eval . fst) x, snd x)) . (Tree.prune depth) . gametree

normalizeResult :: (Eval.Value, Result) -> Result
normalizeResult (value, result) =
    Result value pvs
    where
        pvs = if length pvFromRes > 0 then pvFromRes else [Move.Nil]
        pvFromRes = (reverse $ pv result)