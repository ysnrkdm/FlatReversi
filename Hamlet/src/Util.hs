module Util where

import Control.Arrow
import Data.Char
import Debug.Trace

tuLi2 :: (t, t) -> [t]
tuLi2 (a, b) = [a, b]

liTu2 :: [t] -> (t, t)
liTu2 [a, b] = (a, b)

liFirst :: (t -> t) -> [t] -> [t]
liFirst f [a, b] = [f a, b]

liSecond :: (t -> t) -> [t] -> [t]
liSecond f [a, b] = [a, f b]

convCh :: Char -> Char -> Char -> Char
convCh f t c = chr $ ord c - ord f + ord t

atod :: Char -> Char
atod = convCh 'a' '1'
dtoa :: Char -> Char
dtoa = convCh '1' 'a'

--toPos (f, r) = (13 - f) + 17 * (r + 1)
--fromPos = posFile &&& posRank
--
--posFile po = 13 - po `mod` 17
--posRank po = po `div` 17 - 1

height :: Int
height = 8

width :: Int
width = 8

--posFromCoord :: (Int, Int) -> Piece.Pos
posFromCoord (x, y) = (x - 1) + (y - 1) * 8

coordFromPos = posFile &&& posRank

posFile pos = (pos `mod` width) + 1
posRank pos = (pos `div` height) + 1

notationFromPos :: Int -> String
notationFromPos = map intToDigit . tuLi2 . coordFromPos

posFromNotation :: String -> Int
posFromNotation = posFromCoord . liTu2 . map digitToInt

posFromUSI :: String -> Int
posFromUSI = posFromNotation . liFirst atod
usiFromPos :: Int -> String
usiFromPos = liFirst dtoa . notationFromPos

debugValue v = trace (show v) v