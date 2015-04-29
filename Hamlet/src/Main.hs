{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Control.Monad
import Data.Word
import qualified EdaxProtocol

import Control.Applicative
import System.IO

foreign export ccall search :: Word64 -> Word64 -> Int -> Int -> IO Int

search :: Word64 -> Word64 -> Int -> Int -> IO Int
search black white pnsLessThan searchDepth = do
    putStrLn "Called!"
    return $ 10 + 10

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "Usage: (not implement)"
  subs <- words <$> getLine
  case head subs of
    "usi" -> putStrLn "id name hamilcar\nid author ysnrkdm\nusiok" >> EdaxProtocol.commandLoop undefined
    otherwise -> putStrLn "not implemented"
