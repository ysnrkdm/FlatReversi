module Move where

import Piece
import Util

data Mv =
      Nil                   -- pass
    | Mv {
        to :: Piece.Pos,
        col :: Piece.Pc
    } deriving (Eq, Ord)
instance Show Mv where
    show (Mv to col) = Util.posToUSI to
    show Nil = "Nil"