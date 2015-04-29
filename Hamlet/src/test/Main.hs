module Main where
-- friends
import qualified BitBoard
import qualified Piece
import qualified Search
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
            bench "alphabeta 8 depth" $ whnf (Search.alphabeta 8) BitBoard.initialBoard
--            bench "alphabeta 9 depth" $ whnf (Search.alphabeta 9) BitBoard.initialBoard,
--            bench "alphabeta 10 depth" $ whnf (Search.alphabeta 10) BitBoard.initialBoard
            ]
        ]

alphabetaTest = do
    putStrLn $ show $ Search.alphabeta 8 BitBoard.initialBoard
    putStrLn $ show $ Search.alphabeta 9 BitBoard.initialBoard
--    putStrLn $ show $ Search.alphabeta 14 BitBoard.initialBoard
    1 @=? 1