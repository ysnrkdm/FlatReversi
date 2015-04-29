module Piece where

import Data.Ix

-- 0 .. 63 (64 is out of board)
type Pos = Int

data Co = B | W deriving (Eq, Ord, Enum, Ix)
data Pc = Empty | Wall | Pc {co :: Co} deriving (Eq, Ord)

instance Show Co where
    show B = "B"
    show W = "W"

instance Enum Pc where
    fromEnum (Pc co) = fromEnum co * 16
    toEnum x = Pc (toEnum $ div x 16)
instance Ix Pc where
    range (p1, p2) = [p1 .. p2]
    inRange (c, c') i = c <= i && i <= c'
    index b @ (c, c') ci
        | inRange b ci = fromEnum ci - fromEnum c
        | otherwise= error $ "Pc.index: out of range " ++ show b ++ show ci
instance Show Pc where
    show Empty = "   "
    show Wall = "XXX"
    show (Pc co) = show co

