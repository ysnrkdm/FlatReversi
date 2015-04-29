module Eval (
    eval,
    Value
) where

import qualified Piece
import qualified BitBoard

type Value = Int

eval :: BitBoard.Bb -> Value
eval board
    | BitBoard.isTerminal board = terminalValue board
    | otherwise = staticEval board

terminalValue :: BitBoard.Bb -> Value
terminalValue board@(BitBoard.Bb black white turn)
    | BitBoard.isTerminal board = BitBoard.getNumPiecesFor board turn
    | otherwise = 0

staticEval :: BitBoard.Bb -> Value
staticEval board = openness board + numPieces board + possibleMoves board

openness :: BitBoard.Bb -> Value
openness board = opennessHelper board $ BitBoard.getBoardForTurn board

opennessHelper :: BitBoard.Bb -> BitBoard.BitBoard -> Value
opennessHelper board 0 = 0
opennessHelper board bb = (BitBoard.numPeripherals board Piece.Empty pos) + (opennessHelper board newBb)
    where
    (pos, newBb) = BitBoard.takeOneAndSetZero bb

numPieces :: BitBoard.Bb -> Value
numPieces board = BitBoard.getNumPiecesFor board (BitBoard.turn board)

possibleMoves :: BitBoard.Bb -> Value
possibleMoves board = BitBoard.getNumPuttablesFor board (BitBoard.turn board)