module MoveGenerator where

import qualified BitBoard
import qualified Move
import qualified Piece
import Debug.Trace

moveGenerationFull :: BitBoard.Bb -> [Move.Mv]
moveGenerationFull bb = moveGenerationHelper colour $ BitBoard.getBoardPuttables bb
    where colour = BitBoard.turn bb

moveGenerationHelper :: Piece.Co -> BitBoard.BitBoard -> [Move.Mv]
moveGenerationHelper colour 0 = []
moveGenerationHelper colour puttables = (Move.Mv movePos (Piece.Pc colour)) : (moveGenerationHelper colour newPuttables)
    where
--    newPuttables = trace (BitBoard.showBitBoard n) n
    (movePos, newPuttables) = BitBoard.takeOneAndSetZero puttables