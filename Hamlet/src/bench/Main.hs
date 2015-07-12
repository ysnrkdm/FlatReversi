module Main where
-- friends
import qualified BitBoard
import qualified Piece
import qualified Search
import qualified ProofNumberSearch
-- GHC

-- libraries
import Text.Printf (printf)
import Criterion.Main

-- std
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List

main :: IO ()
main = do
    print "Running test ..."
--    Test.Framework.defaultMain $ hUnitTestToTests $ TestLabel "alphabetaTest" $ TestCase alphabetaTest
    Criterion.Main.defaultMain [
        bgroup "bench group" [
--            bench "alphabeta 3 depth" $ whnf (Search.alphabeta 3) BitBoard.initialBoard,
--            ,bench "alphabeta 8 depth" $ whnf (Search.alphabeta 8) BitBoard.initialBoard
--            ,bench "alphabeta 9 depth" $ whnf (Search.alphabeta 9) BitBoard.initialBoard
--            ,bench "alphabeta 10 depth" $ whnf (Search.alphabeta 10) BitBoard.initialBoard
            bench "pns 2 depth" $ whnf (ProofNumberSearch.proofNumberSearch Piece.B 10) $
                BitBoard.fromString "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOXOXO---- O",
            bench "pns 2 depth" $ whnf (ProofNumberSearch.proofNumberSearch Piece.B 10) $
                BitBoard.fromString "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX-- O",
            bench "pns 2 depth" $ whnf (ProofNumberSearch.proofNumberSearch Piece.B 10) $
                BitBoard.fromString "OOOOOXO---OOOOO-XXOXXXXX-XXOOXXXXXXXXXXX-XOOOXXXX-OOOO----OOOOO- O"
            ]
        ]