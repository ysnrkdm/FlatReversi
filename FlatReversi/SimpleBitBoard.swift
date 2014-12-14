//
//  SimpleBitBoard.swift
//  ReversiTester
//
//  Created by Kodama Yoshinori on 11/19/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class SimpleBitBoard: Board {
    var black: UInt64 = 0b0100000010 << 27
    var white: UInt64 = 0b1000000001 << 27
    var guide: UInt64 = 0b0

    var _height = 8
    var _width = 8
    func height() -> Int {
        return _height
    }
    func width() -> Int {
        return _width
    }

    func initialize(width: Int, height: Int) {
        // ignors width and height
    }

    func withinBoard(x: Int, y: Int) -> Bool {
        return (0 <= x && x < width() && 0 <= y && y < height())
    }

    func set(color: Pieces, x: Int, y: Int) {
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
            println("Do nothing \(color.toString())")
        }
    }

    func get(x: Int, y: Int) -> Pieces {
//        println("getting \(x),\(y)")
        if !withinBoard(x, y: y) {
            return .None
        }

        let bitwhere: UInt64 = 1 << (UInt64(x) + UInt64(y) * 8)
        let blackExists: Bool = black & bitwhere > 0
        let whiteExists: Bool = white & bitwhere > 0
//        println("\(x),\(y) - \(blackExists), \(whiteExists) in \(black), \(white)")
        if blackExists && whiteExists {
            assertionFailure("Should not reach this code. An cell cannot be occupied by both black and white piece!")
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

    private func reverse(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        var reversibles = getReversible(color, x: x, y: y)
        for (rev_x, rev_y) in reversibles {
            set(color, x: rev_x, y: rev_y)
        }

        return reversibles
    }

    func boardForAll(mapfun: (Pieces -> Pieces)) {
        for y in 0..<height() {
            for x in 0..<width() {
                var p = get(x, y: y)
                set(mapfun(p), x: x, y: y)
            }
        }
    }

    func boardForAll(mapfun: ((Int, Int) -> Pieces)) {
        for y in 0..<height() {
            for x in 0..<width() {
                var p = get(x, y: y)
                set(mapfun(x, y), x: x, y: y)
            }
        }
    }

    func updateGuides(color: Pieces) -> Int {
        // Clear exising guides first
        boardForAll({
            (x: Pieces) -> Pieces in if(x == Pieces.Guide) { return Pieces.Empty } else { return x }
        })

        var ret = 0
        boardForAll({
            (x: Int, y: Int) -> Pieces in if(self.canPut(color, x: x, y: y)) { ++ret; return Pieces.Guide } else { return self.get(x, y: y) }
        })

        return ret
    }

    func put(color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)] {
        if !withinBoard(x, y: y) {
            return []
        }

        let direcs = [1,-1,8,-8,-9,7,9,-7]

        var r: UInt64 = 0
        for direc in direcs {
            let pd = getBitReversible(color, x: x, y: y, direc: direc)
            r |= pd
        }

        if r <= 0 {
            return []
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

        return returnChanges ? listFromBitBoard(r) : []

//        if(withinBoard(x, y: y) && canPut(color, x: x, y: y)) {
//            set(color, x: x, y: y)
//            var reversed = reverse(color, x: x, y: y)
//            if guides {
//                updateGuides(nextTurn(color))
//            }
//            return reversed
//        } else {
//            return []
//        }
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

    // MARK: Query functions
    func getNumBlack() -> Int {
        return pop(black)
    }
    
    func getNumWhite() -> Int {
        return pop(white)
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        if (black | white) & bitWhere(x, y: y) > 0 {
            return false
        }

        let direcs = [1,-1,8,-8,-9,7,9,-7]

        var r: UInt64 = 0
        for direc in direcs {
            r |= getBitPuttables(color, direc: direc)
            if r > 0 {
                return true
            }
        }

        return false
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        let direcs = [1,-1,8,-8,-9,7,9,-7]

        var r: UInt64 = 0
        for direc in direcs {
            r |= getBitPuttables(color, direc: direc)
        }
        return listFromBitBoard(r)
    }

    func isAnyPuttable(color: Pieces) -> Bool {
        let direcs = [1,-1,8,-8,-9,7,9,-7]

        for direc in direcs {
            if getBitPuttables(color, direc: direc) > 0 {
                return true
            }
        }
        return false
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
            assertionFailure("Should not reach this code!")
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
            assertionFailure("Should not reach this code!")
        }

        var rev: UInt64 = 0
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

        var blank: UInt64 = ~(black | white)
        var ret = blank & t

        return ret
    }

    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        let direcs = [1,-1,8,-8,-9,7,9,-7]

        var r: UInt64 = 0
        for direc in direcs {
            let pd = getBitReversible(color, x: x, y: y, direc: direc)
            r |= pd
        }

        return listFromBitBoard(r)
    }

    func getBitReversible(color: Pieces, x: Int, y: Int, direc: Int) -> UInt64 {
//        println("getBitReversible \(color.toString()), \(x), \(y) for \(direc) :: b:\(black) + w:\(white) :\n" + toString())
        var attacker: UInt64
        var attackee: UInt64

        var pp = 0
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
            assertionFailure("Should not reach this code!")
        }

        switch color {
        case .Black:
            attacker = black
            attackee = white & mask
        case .White:
            attacker = white
            attackee = black & mask
        default:
            assertionFailure("Should not reach this code!")
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
//                println(bitBoardToString(m7))
//                println(bitBoardToString(m7 & attacker))
                if (m7 & attacker) != 0 {
                    rev = m1 | m2 | m3 | m4 | m5 | m6
                }
            }
        }

//        println("\(bitBoardToString(rev))")
        return rev
    }

    func isEmpty(x: Int, y: Int) -> Bool {
        return (black & white) & bitWhere(x, y: y) > 0
    }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        let peripherals_row: [UInt64] = [
            0b00000011, 0b00000111, 0b00001110, 0b00011100, 0b00111000, 0b01110000, 0b11100000, 0b11000000
        ]

        let peripherals_x: UInt64 = peripherals_row[x]
        var peripherals_xs : UInt64 = peripherals_x << 16 + peripherals_x << 8 + peripherals_x

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

        if color == .Black {
            return pop(black & peripherals)
        } else if color == .White {
            return pop(white & peripherals)
        } else if color == .Empty {
            let empty_cells = (black | white) ^ 0xFFFFFFFFFFFFFFFF
            return pop(empty_cells & peripherals)
        } else {
            return 0
        }
    }

    func hash() -> (UInt64, UInt64) {
        return (black, white)
    }

    // MARK: Bitwise operations
    func pop(i:UInt64) -> Int {
        var x = i
        x = (x & 0x5555555555555555) + ((x >>  1) & 0x5555555555555555)
        x = (x & 0x3333333333333333) + ((x >>  2) & 0x3333333333333333)
        x = (x & 0x0F0F0F0F0F0F0F0F) + ((x >>  4) & 0x0F0F0F0F0F0F0F0F)
        x = (x & 0x00FF00FF00FF00FF) + ((x >>  8) & 0x00FF00FF00FF00FF)
        x = (x & 0x0000FFFF0000FFFF) + ((x >> 16) & 0x0000FFFF0000FFFF)
        x = (x & 0x00000000FFFFFFFF) + ((x >> 32) & 0x00000000FFFFFFFF)

        return Int(x)
    }

    // MARK: Utility functions
    func clone() -> Board {
        let nblack = self.black
        let nwhite = self.white
        var ret = SimpleBitBoard()
        ret.black = nblack
        ret.white = nwhite
        ret._height = self.height()
        ret._width = self.width()

        return ret
    }

    func bitBoardToString(x: UInt64) -> String {
        var ret = ""
        for iy in 0..<height() {
            for ix in 0..<width() {
                let bitwhere = bitWhere(ix, y: iy)
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

    func bitWhere(x: Int, y: Int) -> UInt64 {
        return 1 << (UInt64(x) + UInt64(y) * 8)
    }

    func toString() -> String {
        var ret = ""
        for var y = 0; y < self.height(); ++y {
            for var x = 0; x < self.width(); ++x {
                var p = get(x, y: y)
                var s = p.toString()
                ret += " " + s + " "
            }
            ret += "\n"
        }
        
        return ret
    }
}