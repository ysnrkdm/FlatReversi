module BitBoard where

import qualified Piece
import qualified Move
import qualified Util

import Data.Word
import Data.Bits
import Data.Bits.Extras
import Data.Char
import Control.Exception
import Data.List(foldl')
import Debug.Trace

type BitBoard = Word64
emptyBoard :: BitBoard
emptyBoard = 0

data Bb = Bb {black :: BitBoard, white :: BitBoard, turn :: Piece.Co}

instance Show Bb where
    show bb = showBitBoards bb

showBitBoards :: Bb -> String
showBitBoards bb@(Bb _ _ turn) =
    "Turn : " ++ (if turn == Piece.B then "Black" else "White") ++
        "\r\n   A  B  C  D  F  G  H  I " ++
        showBitBoardsHelper bb puttables (8*8-1)
        where puttables = getPuttables bb turn

showBitBoardsHelper _ _ (-1) = ""
showBitBoardsHelper bb@(Bb black white turn) puttables n =
    a ++ markAtN ++ (showBitBoardsHelper bb puttables (n-1))
        where
            markAtN =
                case blackBitAtN == 0 && whiteBitAtN == 0 && puttablesBitAtN == 0 of
                    True -> " - "
                    False
                        | blackBitAtN > 0 -> " O "
                        | whiteBitAtN > 0 -> " @ "
                        | puttablesBitAtN > 0 && turn == Piece.B -> " b "
                        | puttablesBitAtN > 0 && turn == Piece.W -> " w "
            blackBitAtN = (black .&. (bit (63 - n)))
            whiteBitAtN = (white .&. (bit (63 - n)))
            puttablesBitAtN = (puttables .&. (bit (63 - n)))
            a = if (mod (n+1) 8) == 0 then "\r\n" ++ (show $ 8 - (n `div` 8)) ++ " " else ""

showBitBoard :: BitBoard -> String
showBitBoard bb =
        "   0  1  2  3  4  5  6  7 " ++
        showBitBoardHelper bb (8*8-1)

showBitBoardHelper _ (-1) = ""
showBitBoardHelper bb n =
        case blackBitAtN == 0 of
            True -> a ++ " - " ++ (showBitBoardHelper bb (n-1))
            False -> a ++ " O " ++ (showBitBoardHelper bb (n-1))
        where
            blackBitAtN = (bb .&. (bit (63 - n)))
            a = if (mod (n+1) 8) == 0 then "\r\n" ++ (show $ 8 - (n `div` 8) - 1) ++ " " else ""

data Mvs = Mvs {moves :: BitBoard}

initialBoard :: Bb
initialBoard = (Bb 0x0000000810000000 0x0000001008000000 Piece.B)

--height :: Int
--height = 8
--
--width :: Int
--width = 8
--
--posFromCoord :: Int -> Int -> Piece.Pos
--posFromCoord x y = x + y * 8
--
--coordFromPos :: Piece.Pos -> (Int, Int)
--coordFromPos pos = (mod pos width, div pos height)

getBoardForPlayer :: Bb -> Piece.Co -> BitBoard
getBoardForPlayer (Bb black _ _) Piece.B = black
getBoardForPlayer (Bb _ white _) Piece.W = white

getBoardForTurn :: Bb -> BitBoard
getBoardForTurn (Bb black _ Piece.B) = black
getBoardForTurn (Bb _ white Piece.W) = white

withinBoard :: Bb -> Piece.Pos -> Bool
withinBoard _ pos = pos < (Util.width * Util.height)

bitwhere :: Piece.Pos -> BitBoard
bitwhere = shift 1

set :: Bb -> Piece.Pc -> Piece.Pos -> Bb
set (Bb black white turn) (Piece.Pc Piece.B) pos =
    (Bb (black .|. b) (white .&. (complement b)) turn)
    where b = bitwhere pos
set (Bb black white turn) (Piece.Pc Piece.W) pos =
    (Bb (black .&. (complement b)) (white .|. b) turn)
    where b = bitwhere pos
set (Bb black white turn) Piece.Empty pos =
    (Bb (black .&. (complement b)) (white .&. (complement b)) turn)
    where b = bitwhere pos

get :: Bb -> Piece.Pos -> Piece.Pc
get bb@(Bb black white turn) pos =
    if blackExists then (Piece.Pc Piece.B) else if whiteExists then (Piece.Pc Piece.W) else (Piece.Empty)
    where
        b = bitwhere $ assert (withinBoard bb pos) pos
        blackExists = black .&. b > 0
        whiteExists = white .&. b > 0

data Direc = L | R | U | D | LU | RU | LD | RD deriving (Show)
instance Enum Direc where
    fromEnum L  = -1
    fromEnum R  =  1
    fromEnum U  = -8
    fromEnum D  =  8
    fromEnum LU = -9
    fromEnum RU = -7
    fromEnum LD =  7
    fromEnum RD =  9
    toEnum (-1) = L
    toEnum ( 1) = R
    toEnum (-8) = U
    toEnum ( 8) = D
    toEnum (-9) = LU
    toEnum (-7) = RU
    toEnum ( 7) = LD
    toEnum ( 9) = RD

directions :: [Direc]
directions = [L, R, U, D, LU, RU, LD, RD]

motionMaskFromDirec :: Direc -> BitBoard
-- Horizontal
motionMaskFromDirec   L  = 0x7e7e7e7e7e7e7e7e
motionMaskFromDirec   R  = 0x7e7e7e7e7e7e7e7e
-- Vertical
motionMaskFromDirec   U  = 0x00ffffffffffff00
motionMaskFromDirec   D  = 0x00ffffffffffff00
-- Diag 1 (UpLeft -> DownRight)
motionMaskFromDirec   LU = 0x7e7e7e7e7e7e7e7e
motionMaskFromDirec   RD = 0x7e7e7e7e7e7e7e7e
-- Diag 2 (UpRight -> DownLeft)
motionMaskFromDirec   RU = 0x7e7e7e7e7e7e7e7e
motionMaskFromDirec   LD = 0x7e7e7e7e7e7e7e7e

shiftRU :: Bits a => a -> Int -> a
shiftRU a nShift = if nShift >= 0 then a `shiftR` nShift else a `shiftL` (-nShift)

getBitReversibles :: Bb -> Piece.Co -> Piece.Pos -> Direc -> BitBoard
getBitReversibles (Bb black white turn) colour position direc =
    rev
    where
        pos = bitwhere position
        (attacker, attackee) =
            case colour of
                Piece.B -> (black, white .&. (motionMaskFromDirec direc))
                Piece.W -> (white, black .&. (motionMaskFromDirec direc))
        direcInt = fromEnum direc
        m1 = shiftRU pos direcInt
        m2 = shiftRU m1 direcInt
        m3 = shiftRU m2 direcInt
        m4 = shiftRU m3 direcInt
        m5 = shiftRU m4 direcInt
        m6 = shiftRU m5 direcInt
        m7 = shiftRU m6 direcInt
        rev =
            if m1 .&. attackee > 0 then
                if      m2 .&. attackee == 0 && m2 .&. attacker > 0 then m1
                else if m3 .&. attackee == 0 && m3 .&. attacker > 0 then m1 .|. m2
                else if m4 .&. attackee == 0 && m4 .&. attacker > 0 then m1 .|. m2 .|. m3
                else if m5 .&. attackee == 0 && m5 .&. attacker > 0 then m1 .|. m2 .|. m3 .|. m4
                else if m6 .&. attackee == 0 && m6 .&. attacker > 0 then m1 .|. m2 .|. m3 .|. m4 .|. m5
                else if                         m7 .&. attacker > 0 then m1 .|. m2 .|. m3 .|. m4 .|. m5.|. m6
                else emptyBoard
            else emptyBoard

getReversibles :: Bb -> Piece.Co -> Piece.Pos -> BitBoard
getReversibles bb colour position = foldl' (\x y -> x .|. (getBitReversibles bb colour position y)) emptyBoard directions

put :: Bb -> Piece.Pc -> Piece.Pos -> (Bb, Mvs)
put bb@(Bb black white turn) (Piece.Pc colour) position =
    ((Bb newBlack newWhite newTurn), moves)
    where
        pos = assert (position < 64) position
        r = getReversibles bb colour position
        putAt = bitwhere pos
        (newBlack, newWhite) = case colour of
            Piece.B -> (black `xor` (putAt .|. r), white `xor` r)
            Piece.W -> (black `xor` r, white `xor` (putAt .|. r))
        newTurn =
            case turn of
                Piece.B -> Piece.W
                Piece.W -> Piece.B
        moves = (Mvs r)

isPieceAt :: Bb -> Piece.Pc -> Piece.Pos -> Bool
isPieceAt (Bb black _ _) (Piece.Pc Piece.B) pos = black .&. (bitwhere pos) > 0
isPieceAt (Bb _ white _) (Piece.Pc Piece.W) pos = white .&. (bitwhere pos) > 0

isEmptyAt :: Bb -> Piece.Pos -> Bool
isEmptyAt (Bb black white _) pos = (black .|. white) .&. (bitwhere pos) == 0

getBitPuttables :: Bb -> Piece.Co -> Direc -> BitBoard
getBitPuttables (Bb black white _) colour direc =
    ret
        where
            (attacker, attackee) =
                case colour of
                    Piece.B -> (black, white .&. (motionMaskFromDirec direc))
                    Piece.W -> (white, black .&. (motionMaskFromDirec direc))
            direcInt = fromEnum direc
            t = bt
                where
                mb1 = attackee .&. (shiftRU attacker direcInt)
                mb2 = mb1 .|. (attackee .&. (shiftRU mb1 direcInt))
                mb3 = mb2 .|. (attackee .&. (shiftRU mb2 direcInt))
                mb4 = mb3 .|. (attackee .&. (shiftRU mb3 direcInt))
                mb5 = mb4 .|. (attackee .&. (shiftRU mb4 direcInt))
                mb6 = mb5 .|. (attackee .&. (shiftRU mb5 direcInt))
                bt = (shiftRU mb6 direcInt)
            blank = complement $ black .|. white
            ret = t .&. blank

getPuttables :: Bb -> Piece.Co -> BitBoard
getPuttables bb colour = foldr (\x y -> y .|. (getBitPuttables bb colour x)) emptyBoard directions

getBoardPuttables :: Bb -> BitBoard
getBoardPuttables bb@(Bb _ _ colour) = foldr (\x y -> y .|. (getBitPuttables bb colour x)) emptyBoard directions

isAnyPuttable :: Bb -> Piece.Co -> Bool
isAnyPuttable bb colour = foldr (\x y -> y || ((getBitPuttables bb colour x) > 0)) False directions

getNumPuttablesFor :: Bb -> Piece.Co -> Int
getNumPuttablesFor bb colour = popCount $ getPuttables bb colour

getNumPiecesBlack :: Bb -> Int
getNumPiecesBlack (Bb black _ _) = popCount black

getNumPiecesWhite :: Bb -> Int
getNumPiecesWhite (Bb _ white _) = popCount white

getNumPiecesFor :: Bb -> Piece.Co -> Int
getNumPiecesFor bb colour =
    case colour of
        Piece.B -> getNumPiecesBlack bb
        Piece.W -> getNumPiecesWhite bb

getNumVacant :: Bb -> Int
getNumVacant bb = 64 - getNumPiecesBlack bb - getNumPiecesWhite bb

isTerminal :: Bb -> Bool
isTerminal bb = (getNumVacant bb == 0) || not(isAnyPuttable bb Piece.B) || not(isAnyPuttable bb Piece.W)

canPut :: Bb -> Piece.Co -> Piece.Pos -> Bool
canPut bb colour pos = (isEmptyAt bb pos) && ((getPuttables bb colour) .&. (bitwhere pos)) > 0

canPutMove :: Bb -> Move.Mv -> Bool
canPutMove bb Move.Nil = True
canPutMove bb (Move.Mv pos (Piece.Pc colour)) = (isEmptyAt bb pos) && ((getPuttables bb colour) .&. (bitwhere pos)) > 0

numPeripherals :: Bb -> Piece.Pc -> Piece.Pos -> Int
numPeripherals (Bb black white _) piece pos =
    popCount $ table
    where
        table = case piece of
            Piece.Pc Piece.B -> black .&. peripherals
            Piece.Pc Piece.W -> white .&. peripherals
            Piece.Empty -> (black .|. white) `xor` 0xFFFFFFFFFFFFFFFF
        peripherals = peripherals_xxs .&. ((bitwhere pos) `xor` 0xFFFFFFFFFFFFFFFF)
        (x, y) = Util.coordFromPos pos
        peripherals_xxs = case y of
            0 -> peripherals_x `shiftL` 8 + peripherals_x
            1 -> peripherals_xs
            2 -> peripherals_xs `shiftL` (8 * 1)
            3 -> peripherals_xs `shiftL` (8 * 2)
            4 -> peripherals_xs `shiftL` (8 * 3)
            5 -> peripherals_xs `shiftL` (8 * 4)
            6 -> peripherals_xs `shiftL` (8 * 5)
            7 -> peripherals_xs `shiftL` (8 * 6)
        peripherals_xs = peripherals_x `shiftL` 16 + peripherals_x `shiftL` 8 + peripherals_x
        peripherals_x = case x of
            0 -> 0x03 -- 0b00000011
            1 -> 0x07 -- 0b00000111
            2 -> 0x0E -- 0b00001110
            3 -> 0x1C -- 0b00011100
            4 -> 0x38 -- 0b00111000
            5 -> 0x70 -- 0b01110000
            6 -> 0xE0 -- 0b11100000
            7 -> 0xC0 -- 0b11000000


move :: BitBoard.Bb -> Move.Mv -> BitBoard.Bb
move bb (Move.Mv to piece) = fst $ put bb piece to

takeOneFromBitBoard :: BitBoard -> Piece.Pos
takeOneFromBitBoard bb = rank bb - 1

takeOneAndSetZero :: BitBoard -> (Piece.Pos, BitBoard)
takeOneAndSetZero bb =
    (mssb, newBb)
    where
        mssb = takeOneFromBitBoard bb
        newBb = bb `xor` (bitwhere mssb)
