module Eval (
    eval,
    Value
) where

import qualified Piece
import qualified BitBoard

type Value = Double
type Coef = Double

--fv :: Int -> Va
--fv i = (fromIntegral :: Int8 -> Int) . (fromIntegral :: Word8 -> Int8) $ BS.index fvbin i

--fvbin :: BS.ByteString
--fvbin = unsafePerformIO $ BS.readFile "./fv.bin"

eval :: BitBoard.Bb -> Value
eval board
    | BitBoard.isTerminal board = terminalValue board
    | otherwise = staticEval board

terminalValue :: BitBoard.Bb -> Value
terminalValue board@(BitBoard.Bb black white turn)
    | BitBoard.isTerminal board = fromIntegral $ (BitBoard.getNumPiecesFor board turn) - (BitBoard.getNumPiecesFor board (BitBoard.oppositeSide turn))
    | otherwise = 0.0

staticEval :: BitBoard.Bb -> Value
staticEval board =
    (opennessCoef progress + openness board) +
    (numPiecesCoef progress + numPieces board) +
    (possibleMovesCoef progress + possibleMoves board)
    where
        progress = 64 - BitBoard.getNumVacant board

coefIndex :: Int -> Int -> Int
coefIndex progress pPerBuckets = floor $ (fromIntegral progress) / (fromIntegral pPerBuckets)

opennessCoef :: Int -> Coef
opennessCoef progress = 1.0 * [8, 6, 6, 3, 3] !! (coefIndex progress 15)

numPiecesCoef :: Int -> Coef
numPiecesCoef progress = 1.0 * [0, 1, 1, 10, 10] !! (coefIndex progress 15)

possibleMovesCoef :: Int -> Coef
possibleMovesCoef progress = 1.0 * [9, 3, 4, 1, 1] !! (coefIndex progress 15)

openness :: BitBoard.Bb -> Value
openness board = opennessHelper board $ BitBoard.getBoardForTurn board

opennessHelper :: BitBoard.Bb -> BitBoard.BitBoard -> Value
opennessHelper board 0 = 0
opennessHelper board bb = (fromIntegral $ BitBoard.numPeripherals board Piece.Empty pos) + (opennessHelper board newBb)
    where
    (pos, newBb) = BitBoard.takeOneAndSetZero bb

numPieces :: BitBoard.Bb -> Value
numPieces board = fromIntegral $ BitBoard.getNumPiecesFor board (BitBoard.turn board)

possibleMoves :: BitBoard.Bb -> Value
possibleMoves board = fromIntegral $ BitBoard.getNumPuttablesFor board (BitBoard.turn board)