module Dfpn where

import qualified Move
import qualified BitBoard
import qualified Piece
import Control.Monad
import qualified Util
import qualified Tree
import Debug.Trace
import Data.Ord
import Data.List
import qualified MoveGenerator

data NodeType = Or | And deriving Show

data ProofResult = BlackWin | WhiteWin | Draw | None deriving Show

--data Proof = Proof {result :: ProofResult, pv :: Maybe [Move.Mv]} deriving Show

--data ProofType = Proven | Disproven | Unknown deriving (Show, Eq, Ord)

data ProofDisproofNumber = LeafProven | LeafDisproven | LeafUnknown | ProofDisproofNumber {proof :: Int, disproof :: Int} deriving (Show, Eq, Ord)

data Result = Result {va :: ProofDisproofNumber, pv :: [Move.Mv]} deriving (Eq, Ord)
instance Show Result where
    show (Result va pv) = "va: " ++ (show va) ++ ", pv : " ++ (show pv)

infNum :: Int
infNum = maxBound :: Int

conv :: Move.Mv -> Result -> Result
conv mv res = Result (ProofDisproofNumber ((disproof . va) res) ((proof . va) res)) (mv : pv res)

moves :: (BitBoard.Bb, Result) -> [(BitBoard.Bb, Result)]
moves (bd, result) =
    map (\ x -> (BitBoard.move bd x, (conv x result))) $ MoveGenerator.moveGenerationFull bd

gametree :: BitBoard.Bb -> Tree.Tree (BitBoard.Bb, Result)
gametree p = Tree.reptree moves $ (p, Result (ProofDisproofNumber 0 0) [])

orize :: Tree.Tree Result -> Result
orize = selectMostProvenNodeOr . orize'

orize' :: Tree.Tree Result -> [Result]
orize' Tree.Node {Tree.node = n, Tree.childNodes = []} = n : []
orize' Tree.Node {Tree.node = _, Tree.childNodes = c} = map selectMostProvenNodeAnd $ map andize' c

selectMostProvenNodeAnd :: [Result] -> Result
selectMostProvenNodeAnd pdns =
    selectMostProvenNodeAnd' 0 0 pdns []
    where
        selectMostProvenNodeAnd' pn dn [] ans = Result (ProofDisproofNumber pn dn) ans
        selectMostProvenNodeAnd' pn dn (x:rest) ans
            | (pn == infNum) && (dn == infNum) = Result (ProofDisproofNumber infNum infNum) ans
            | otherwise = case x of
                Result LeafProven pv -> selectMostProvenNodeAnd' (infNum) (dn) rest pv
                Result LeafDisproven pv -> selectMostProvenNodeAnd' (pn) (infNum) rest pv
                Result LeafUnknown pv -> selectMostProvenNodeAnd' (pn+1) (minimum [dn, 1]) rest pv
                Result (ProofDisproofNumber xpn xdn) pv -> selectMostProvenNodeAnd' (pn+xpn) (minimum [dn,xdn]) rest pv

selectMostProvenNodeOr :: [Result] -> Result
selectMostProvenNodeOr pdns =
    selectMostProvenNodeOr' 0 0 pdns []
    where
        selectMostProvenNodeOr' pn dn [] ans = Result (ProofDisproofNumber pn dn) ans
        selectMostProvenNodeOr' pn dn (x:rest) ans
            | (pn == infNum) && (dn == infNum) = Result (ProofDisproofNumber infNum infNum) ans
            | otherwise = case x of
                Result LeafProven pv -> selectMostProvenNodeOr' (infNum) (dn) rest pv
                Result LeafDisproven pv -> selectMostProvenNodeOr' (pn) (infNum) rest pv
                Result LeafUnknown pv -> selectMostProvenNodeOr' (minimum [pn, 1]) (dn+1) rest pv
                Result (ProofDisproofNumber xpn xdn) pv -> selectMostProvenNodeOr' (minimum [pn, xpn]) (dn+xdn) rest pv

andize :: Tree.Tree Result -> Result
andize = selectMostProvenNodeAnd . andize'

andize' :: Tree.Tree Result -> [Result]
andize' Tree.Node {Tree.node = n, Tree.childNodes = []} = n : []
andize' Tree.Node {Tree.node = _, Tree.childNodes = c} = map selectMostProvenNodeOr $ map orize' c

evaluate :: Piece.Co -> (BitBoard.Bb, Result) -> Result
evaluate attacker current@(bb@(BitBoard.Bb _ _ turn), ress) =
    case (attacker, declare bb) of
            -- Leaf node
            (Piece.B, BlackWin) -> Result LeafProven (pv ress)
            (Piece.W, WhiteWin) -> Result LeafProven (pv ress)
            (Piece.W, BlackWin) -> Result LeafDisproven (pv ress)
            (Piece.B, WhiteWin) -> Result LeafDisproven (pv ress)
            (_, Draw) -> Result LeafDisproven (pv ress)
            -- Interior node
            (_, None) -> Result LeafUnknown (pv ress)

dfpn :: Piece.Co -> Int -> BitBoard.Bb -> Result
dfpn attacker depth = orize . Tree.maptree (evaluate attacker) . (Tree.prune depth) . gametree

declare :: BitBoard.Bb -> ProofResult
declare bb =
    if BitBoard.isTerminal bb
    then
        if BitBoard.getNumPiecesBlack bb > BitBoard.getNumPiecesWhite bb then BlackWin
        else if BitBoard.getNumPiecesBlack bb < BitBoard.getNumPiecesWhite bb then WhiteWin
        else Draw
    else None

nodeType :: Piece.Co -> Piece.Co -> NodeType
nodeType turn attacker = if turn == attacker then Or else And
