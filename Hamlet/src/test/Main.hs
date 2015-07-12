module Main where
-- friends
import qualified BitBoard
import qualified Piece
import qualified Search
import qualified ProofNumberSearch
import qualified Util
import qualified Move
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
    Test.Framework.defaultMain $ hUnitTestToTests $ TestList [
        "ProofNumberSearch Test" ~: TestList [
            "pnsTest1" ~: (pnsProven []) @=? (pns b 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-- O"),
            "pnsTest2" ~: (pnsProven []) @=? (pns b 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-- O"),
            "pnsTest3" ~: (pnsProven ["Bh8"]) @=? (pns b 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOX- O"),
            "pnsTest4" ~: (pnsDisproven ["Bf8"]) @=? (pns b 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX--- O"),
            "pnsTest5" ~: (pnsDisproven ["Bg8"]) @=? (pns b 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX-- O"),
            "pnsTest6" ~: (pnsDisproven ["Bh8"]) @=?(pns b 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX- O"),
            "pnsTest7" ~: (pnsDisproven ["Bh8"]) @=? (pns b 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOXXXXXXXOXXXXXOX- O"),
            "pnsTest8" ~: (pnsProven ["Bh8"]) @=? (pns b 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOXOOOOOOOXOOOOXXX- O")
        ],
        "AlphabeaSearch Test" ~: TestList [
            "alphabetaTest" ~: alphabetaTest
        ]
        ]

pns a q r = ProofNumberSearch.proofNumberSearch a q $ BitBoard.fromString r
b = Piece.B
w = Piece.W

alphabetaTest = do
    putStrLn $ show $ Search.alphabeta 8 BitBoard.initialBoard
    putStrLn $ show $ Search.alphabeta 9 BitBoard.initialBoard
    1 @=? 1

pnsProven mvs = ProofNumberSearch.Result (ProofNumberSearch.ProofDisproofNumber 9223372036854775807 0) (map Move.fromString mvs)
pnsDisproven mvs = ProofNumberSearch.Result (ProofNumberSearch.ProofDisproofNumber 0 9223372036854775807) (map Move.fromString mvs)
pnsZeros mvs = ProofNumberSearch.Result (ProofNumberSearch.ProofDisproofNumber 0 0) (map Move.fromString mvs)
