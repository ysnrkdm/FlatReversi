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
    show (Mv to col) = (show col) ++ (Util.usiFromPos to)
    show Nil = "Nil"

fromString :: String -> Mv
fromString (x:y:z) =
    Move.Mv (Util.posFromUSI (y:z)) (if x == 'B' then (Piece.Pc Piece.B) else (Piece.Pc Piece.W))