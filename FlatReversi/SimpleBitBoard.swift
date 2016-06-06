//
//  SimpleBitBoard.swift
//  ReversiTester
//
//  Created by Kodama Yoshinori on 11/19/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

typealias Moves = UInt64

func isEmpty(m: Moves) -> Bool {
    return m <= 0
}

//let bsfMagicTable = [
//     0, 47,  1, 56, 48, 27,  2, 60,
//    57, 49, 41, 37, 28, 16,  3, 61,
//    54, 58, 35, 52, 50, 42, 21, 44,
//    38, 32, 29, 23, 17, 11,  4, 62,
//    46, 55, 26, 59, 40, 36, 15, 53,
//    34, 51, 20, 43, 31, 22, 10, 45,
//    25, 39, 14, 33, 19, 30,  9, 24,
//    13, 18,  8, 12,  7,  6,  5, 63
//]

let magic: UInt64 = 0x03f79d71b4cb0a89

//func bitScanForward(b: Moves) -> Int {
//    let binv = (b ^ (b - 1))
//    let bm = binv &* magic
//    let bmShifted = (bm) >> 58
//    let index = Int(bmShifted)
//    return bsfMagicTable[index]
//}

@_silgen_name("_bitScanForward")
    func _bitScanForward(_: UInt64) -> UInt

func bitScanForward(board: UInt64) -> Int {
    return Int(_bitScanForward(board))
}

func xOrBitWhere(b: Moves, nthBit: Int) -> UInt64 {
    return b ^ bitWhere(nthBit)
}

func bitWhere(x: Int) -> UInt64 {
    return 1 << UInt64(x)
}

func bitWhere(x: Int, y: Int) -> UInt64 {
    return 1 << (UInt64(x) + UInt64(y) * 8)
}

@_silgen_name("_bitPop")
    func _bitPop(_: UInt64) -> UInt

func pop(i:UInt64) -> Int {
    return Int(_bitPop(i))
}

func stringFromBitBoard(x: UInt64) -> String {
    var ret = ""
    for iy in 0..<8 {
        for ix in 0..<8 {
            let bitwhere = bitWhere(ix + iy * 8)
            var s = "."
            if bitwhere & x > 0 {
                s = "*"
            }
            ret += " " + s + " "
        }
        ret += "\n"
    }
    return ret
}

let direcs = [1,-1,8,-8,-9,7,9,-7]

func == (lhs: BitBoard, rhs: BitBoard) -> Bool {
    return lhs.black == rhs.black && lhs.white == rhs.white
}

struct BitBoard : Hashable, Equatable {
    var black: UInt64 = 0b0100000010 << 27
    var white: UInt64 = 0b1000000001 << 27
    var guide: UInt64 = 0b0

    var hashValue: Int {
        get {
            let b = Int(black % UInt64(Int.max))
            let w = Int(white % UInt64(Int.max))
            let hash = b &+ w &* 17
            return hash
        }
    }

    func getBoardForPlayer(forPlayer: Pieces) -> UInt64 {
        switch forPlayer {
        case .Black:
            return black
        case .White:
            return white
        default:
            fatalError("Please specify black or white!")
        }
    }

    func height() -> Int {
        return 8
    }
    func width() -> Int {
        return 8
    }

    func withinBoard(x: Int, y: Int) -> Bool {
        return (0 <= x && x < height() && 0 <= y && y < width())
    }

    mutating func set(color: Pieces, x: Int, y: Int) {
        let bitwhere: UInt64 = 1 << (UInt64(x) + UInt64(y) * 8)
        switch color {
        case .Black:
            black = black | bitwhere
            white = white & (~bitwhere)
        case .White:
            white = white | bitwhere
            black = black & (~bitwhere)
        case .Guide:
            guide = guide | bitwhere
        case .Empty:
            black = black & (~bitwhere)
            white = white & (~bitwhere)
            guide = guide & (~bitwhere)
        default:
            print("Do nothing \(color.toString())")
        }
    }

    func get(x: Int, y: Int) -> Pieces {
        if !withinBoard(x, y: y) {
            return .None
        }

        let bitwhere: UInt64 = 1 << (UInt64(x) + UInt64(y) * 8)
        let blackExists: Bool = black & bitwhere > 0
        let whiteExists: Bool = white & bitwhere > 0

        if blackExists && whiteExists {
            fatalError("Should not reach this code. An cell cannot be occupied by both black and white piece!")
        } else if blackExists && !whiteExists {
            return .Black
        } else if !blackExists && whiteExists {
            return .White
        } else if guide & bitwhere > 0 {
            return .Guide
        } else {
            return .Empty
        }
    }

    mutating func put(color: Pieces, x: Int, y: Int, guides: Bool) -> Moves {
        if !withinBoard(x, y: y) {
            return 0x0
        }

        var r: UInt64 = 0
        for direc in direcs {
            let pd = getBitReversible(color, x: x, y: y, direc: direc)
            r |= pd
        }

        if r <= 0 {
            return 0x0
        }

        let putAt = bitWhere(x, y: y)

        switch color {
        case .Black:
            black = black ^ (putAt | r)
            white = white ^ r
        case .White:
            black = black ^ r
            white = white ^ (putAt | r)
        default:
            assertionFailure("Should not reach this code!")
        }

        return r
    }

    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool {
        switch piece {
        case .Black:
            return bitWhere(x, y: y) & black > 0
        case .White:
            return bitWhere(x, y: y) & white > 0
        case .Guide:
            return bitWhere(x, y: y) & guide > 0
        case .Empty:
            return bitWhere(x, y: y) & (black | white) > 0
        default:
            return false
        }
    }

    func isEmpty(x: Int, y: Int) -> Bool {
        return (black & white) & bitWhere(x, y: y) > 0
    }

    func isAnyPuttable(color: Pieces) -> Bool {
        for direc in direcs {
            if getBitPuttables(color, direc: direc) > 0 {
                return true
            }
        }

        return false
    }

    func getNumBlack() -> Int {
        return pop(black)
    }

    func getNumWhite() -> Int {
        return pop(white)
    }

    func getNumVacant() -> Int {
        return 64 - getNumBlack() - getNumWhite()
    }

    func isTerminal() -> Bool {
        if getNumVacant() == 0 {
            return true
        }

        if isAnyPuttable(.Black) {
            return false
        }

        if isAnyPuttable(.White) {
            return false
        }

        return true
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        if (black | white) & bitWhere(x, y: y) > 0 {
            return false
        }

        for direc in direcs {
            if getBitPuttables(color, direc: direc) & bitWhere(x, y: y) > 0 {
                return true
            }
        }

        return false
    }

    func getBitPuttables(color: Pieces, direc: Int) -> UInt64 {
        var mask: UInt64

        if direc == 1 || direc == -1 {
            mask = 0x7e7e7e7e7e7e7e7e
        } else if direc == 8 || direc == -8 {
            mask = 0x00ffffffffffff00
        } else if direc == 7 || direc == -9 {
            mask = 0x7e7e7e7e7e7e7e7e
        } else if direc == 9 || direc == -7 {
            mask = 0x7e7e7e7e7e7e7e7e
        } else {
            fatalError("Should not reach this code!")
        }

        var attacker: UInt64
        var attackee: UInt64

        switch color {
        case .Black:
            attacker = black
            attackee = white & mask
        case .White:
            attacker = white
            attackee = black & mask
        default:
            fatalError("Should not reach this code!")
        }

        var t: UInt64 = 0
        if direc >= 0 {
            let ui64_direc: UInt64 = UInt64(direc)
            t = attackee & (attacker >> ui64_direc)
            t |= attackee & (t >> ui64_direc)
            t |= attackee & (t >> ui64_direc)
            t |= attackee & (t >> ui64_direc)
            t |= attackee & (t >> ui64_direc)
            t |= attackee & (t >> ui64_direc)
            t = (t >> ui64_direc)
        } else {
            let ui64_direc: UInt64 = UInt64(-direc)
            t = attackee & (attacker << ui64_direc)
            t |= attackee & (t << ui64_direc)
            t |= attackee & (t << ui64_direc)
            t |= attackee & (t << ui64_direc)
            t |= attackee & (t << ui64_direc)
            t |= attackee & (t << ui64_direc)
            t = (t << ui64_direc)
        }

        let blank: UInt64 = ~(black | white)
        let ret = blank & t
        
        return ret
    }

    func getPuttables(color: Pieces) -> Moves {
        var r: UInt64 = 0
        for direc in direcs {
            r |= getBitPuttables(color, direc: direc)
        }
        return r
    }

    func getBitReversible(color: Pieces, x: Int, y: Int, direc: Int) -> UInt64 {
        var attacker: UInt64
        var attackee: UInt64

        var mask: UInt64

        if direc == 1 || direc == -1 {
            mask = 0x7e7e7e7e7e7e7e7e
        } else if direc == 8 || direc == -8 {
            mask = 0x00ffffffffffff00
        } else if direc == 7 || direc == -9 {
            mask = 0x7e7e7e7e7e7e7e7e
        } else if direc == 9 || direc == -7 {
            mask = 0x7e7e7e7e7e7e7e7e
        } else {
            fatalError("Should not reach this code!")
        }

        switch color {
        case .Black:
            attacker = black
            attackee = white & mask
        case .White:
            attacker = white
            attackee = black & mask
        default:
            fatalError("Should not reach this code!")
        }

        var m1: UInt64
        var m2: UInt64
        var m3: UInt64
        var m4: UInt64
        var m5: UInt64
        var m6: UInt64
        var m7: UInt64

        let pos: UInt64 = 1 << (UInt64(x) + UInt64(y) * 8)

        var ui64_direc: UInt64
        if direc >= 0 {
            ui64_direc = UInt64(direc)
            m1 = pos >> ui64_direc
            m2 = m1 >> ui64_direc
            m3 = m2 >> ui64_direc
            m4 = m3 >> ui64_direc
            m5 = m4 >> ui64_direc
            m6 = m5 >> ui64_direc
            m7 = m6 >> ui64_direc
        } else {
            ui64_direc = UInt64(-direc)
            m1 = pos << ui64_direc
            m2 = m1 << ui64_direc
            m3 = m2 << ui64_direc
            m4 = m3 << ui64_direc
            m5 = m4 << ui64_direc
            m6 = m5 << ui64_direc
            m7 = m6 << ui64_direc
        }

        var rev: UInt64 = 0

        if (m1 & attackee) != 0 {
            if (m2 & attackee) == 0 {
                if (m2 & attacker) != 0 {
                    rev = m1
                }
            } else if (m3 & attackee) == 0 {
                if (m3 & attacker) != 0 {
                    rev = m1 | m2
                }
            } else if (m4 & attackee) == 0 {
                if (m4 & attacker) != 0 {
                    rev = m1 | m2 | m3
                }
            } else if (m5 & attackee) == 0 {
                if (m5 & attacker) != 0 {
                    rev = m1 | m2 | m3 | m4
                }
            } else if (m6 & attackee) == 0 {
                if (m6 & attacker) != 0 {
                    rev = m1 | m2 | m3 | m4 | m5
                }
            } else {
                if (m7 & attacker) != 0 {
                    rev = m1 | m2 | m3 | m4 | m5 | m6
                }
            }
        }
        
        return rev
    }

    func getReversible(color: Pieces, x: Int, y: Int) -> Moves {
        var r: UInt64 = 0
        for direc in direcs {
            let pd = getBitReversible(color, x: x, y: y, direc: direc)
            r |= pd
        }

        return r
    }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        var peripherals_x: UInt64 = 0
        var peripherals_xs : UInt64 = 0
        switch x {
        case 0:
            peripherals_x = 0b00000011
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 1:
            peripherals_x = 0b00000111
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 2:
            peripherals_x = 0b00001110
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 3:
            peripherals_x = 0b00011100
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 4:
            peripherals_x = 0b00111000
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 5:
            peripherals_x = 0b01110000
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 6:
            peripherals_x = 0b11100000
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        case 7:
            peripherals_x = 0b11000000
            peripherals_xs = peripherals_x << 16 + peripherals_x << 8 + peripherals_x
        default:
            assertionFailure("Should not reach this code!")
        }

        switch y {
        case 0:
            peripherals_xs = peripherals_x << 8 + peripherals_x
        case 1:
            peripherals_xs = peripherals_xs * 1
        case 2:
            peripherals_xs = peripherals_xs << (8 * 1)
        case 3:
            peripherals_xs = peripherals_xs << (8 * 2)
        case 4:
            peripherals_xs = peripherals_xs << (8 * 3)
        case 5:
            peripherals_xs = peripherals_xs << (8 * 4)
        case 6:
            peripherals_xs = peripherals_xs << (8 * 5)
        case 7:
            peripherals_xs = peripherals_xs << (8 * 6)
        default:
            assertionFailure("Should not reach this code!")
        }

        let peripherals = peripherals_xs & (bitWhere(x, y: y) ^ 0xFFFFFFFFFFFFFFFF)

        switch color {
        case .Black:
            return pop(black & peripherals)
        case .White:
            return pop(white & peripherals)
        case .Empty:
            let empty_cells = (black | white) ^ 0xFFFFFFFFFFFFFFFF
            return pop(empty_cells & peripherals)
        default:
            return 0
        }
    }
}

class SimpleBitBoard: FastBitBoard {
    var bb: BitBoard = BitBoard()

    var _height = 8
    var _width = 8

    override init() {
        self.bb = BitBoard()
    }

    init(bitBoard: BitBoard) {
        self.bb = bitBoard
    }

    override func getUnsafeBitBoard() -> BitBoard {
        return bb
    }

    override func height() -> Int {
        return _height
    }
    override func width() -> Int {
        return _width
    }

    override func initialize(width: Int, height: Int) {
        // ignors width and height
    }

    override func withinBoard(x: Int, y: Int) -> Bool {
        return (0 <= x && x < width() && 0 <= y && y < height())
    }

    override func set(color: Pieces, x: Int, y: Int) {
        bb.set(color, x: x, y: y)
    }

    override func get(x: Int, y: Int) -> Pieces {
        return bb.get(x, y: y)
    }

    func boardForAll(mapfun: (Pieces -> Pieces)) {
        for y in 0..<height() {
            for x in 0..<width() {
                let p = get(x, y: y)
                set(mapfun(p), x: x, y: y)
            }
        }
    }

    func boardForAll(mapfun: ((Int, Int) -> Pieces)) {
        for y in 0..<height() {
            for x in 0..<width() {
                set(mapfun(x, y), x: x, y: y)
            }
        }
    }

    override func updateGuides(color: Pieces) -> Int {
        // Clear exising guides first
        boardForAll({
            (x: Pieces) -> Pieces in if(x == Pieces.Guide) { return Pieces.Empty } else { return x }
        })

        var ret = 0
        boardForAll({
            (x: Int, y: Int) -> Pieces in if(self.canPut(color, x: x, y: y)) { ret += 1; return Pieces.Guide } else { return self.get(x, y: y) }
        })

        return ret
    }

    override func put(color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)] {
        if !withinBoard(x, y: y) {
            return []
        }

        let retMoves = bb.put(color, x: x, y: y, guides: guides)

        return returnChanges ? listFromBitBoard(retMoves) : []
    }

    override func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool {
        return bb.isPieceAt(piece, x: x, y: y)
    }

    // MARK: Query functoverride ions
    override func getNumBlack() -> Int {
        return bb.getNumBlack()
    }

    override func getNumWhite() -> Int {
        return bb.getNumWhite()
    }

    override func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        return bb.canPut(color, x: x, y: y)
    }

    override func getPuttables(color: Pieces) -> [(Int, Int)] {
        return listFromBitBoard(bb.getPuttables(color))
    }

    override func isAnyPuttable(color: Pieces) -> Bool {
        return bb.isAnyPuttable(color)
    }

    private func listFromBitBoard(bits: UInt64) -> [(Int, Int)] {
        var ret: [(Int, Int)] = []
        for iy in 0..<height() {
            for ix in 0..<width() {
                let bitwhere: UInt64 = 1 << (UInt64(ix) + UInt64(iy) * 8)
                if bits & bitwhere > 0 {
                    ret.append((ix, iy))
                }
            }
        }
        return ret
    }

    override func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        return listFromBitBoard(bb.getReversible(color, x: x, y: y))
    }

    override func isEmpty(x: Int, y: Int) -> Bool {
        return bb.isEmpty(x, y: y)
    }

    override func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        return bb.numPeripherals(color, x: x, y: y)
    }

    override func isTerminal() -> Bool {
        return bb.isTerminal()
    }

    override func hashValue() -> Int {
        return bb.hashValue
    }

    // MARK: Bitwise operations


    // MARK: Utility functions
    override func clone() -> Board {
        let bb = self.bb
        let ret = SimpleBitBoard()
        ret.bb = bb
        ret._height = self.height()
        ret._width = self.width()

        return ret
    }

    override func cloneBitBoard() -> FastBitBoard {
        let bb = self.bb
        let ret = SimpleBitBoard()
        ret.bb = bb
        ret._height = self.height()
        ret._width = self.width()

        return ret
    }

    override func toString() -> String {
        var ret = ""
        for y in 0..<self.height() {
            for x in 0..<self.width() {
                let p = get(x, y: y)
                let s = p.toString()
                ret += " " + s + " "
            }
            ret += "\n"
        }
        
        return ret
    }
}