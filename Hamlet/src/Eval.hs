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
staticEval board = sum
    [
        positionCoef progress * position board,
        opennessCoef progress * openness board,
        numPiecesCoef progress * numPieces board,
        possibleMovesCoef progress * possibleMoves board
    ]
    where
        progress = 64 - BitBoard.getNumVacant board

position :: BitBoard.Bb -> Value
position board@(BitBoard.Bb _ _ colour) = sum -- $ map fromIntegral
    [
        99  * boardMap 0x8100000000000081,      -- corner A
        1.6 * boardMap 0x4281000000008142,      -- corner neighbor H/V B
        -5  * boardMap 0x0042000000004200,      -- corner neighbor diag C
        7.5 * boardMap 0x2400810000810024,      -- corner neighbors' neighbor D
        6.1 * boardMap 0x1800008181000018,      -- E
        4.3 * boardMap 0x003C424242423C00,      -- F
        4.8 * boardMap 0x0000240000240000,      -- G
        5.0 * boardMap 0x0000183C3C180000       -- H
    ]
    where
        boardMap bmap = fromIntegral $ BitBoard.getNumPiecesWithMaskFor board bmap colour

positionCoef :: Int -> Coef
positionCoef progress = 1.0 * [2, 4, 6, 8, 8] !! (coefIndex progress 15)

coefIndex :: Int -> Int -> Int
coefIndex progress pPerBuckets = floor $ (fromIntegral progress) / (fromIntegral pPerBuckets)

opennessCoef :: Int -> Coef
opennessCoef progress = 1.0 * [8, 6, 6, 3, 3] !! (coefIndex progress 15)

numPiecesCoef :: Int -> Coef
numPiecesCoef progress = 1.0 * [0, 1, 50, 200, 9999] !! (coefIndex progress 15)

possibleMovesCoef :: Int -> Coef
possibleMovesCoef progress = 1.0 * [20, 15, 3.2, 1.1, 1.1] !! (coefIndex progress 15)

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