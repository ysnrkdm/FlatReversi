module Main where
import Control.Monad
import Data.Word
import qualified EdaxProtocol

import Control.Applicative
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    EdaxProtocol.commandLoop (EdaxProtocol.Learn EdaxProtocol.TDLambda) undefined
    putStrLn "sdfasdf"