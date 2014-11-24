//
//  SimpleBitBoard.swift
//  ReversiTester
//
//  Created by Kodama Yoshinori on 11/19/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class SimpleBitBoard: Board {
    var black: UInt64 = 0b1000000001 << 27
    var white: UInt64 = 0b0100000010 << 27

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
            println("Guide not yet implemented")
        case .Empty:
            black = black & (~bitwhere)
            white = white & (~bitwhere)
        default:
            println("Do nothing \(color.toString())")
        }
    }

    func get(x: Int, y: Int) -> Pieces {
//        println("getting \(x),\(y)")
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
        } else {
            return .Empty
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
        return (get(x, y: y) != .White) && (get(x, y: y) != .Black) && (getReversible(color, x: x, y: y).count > 0)
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        let direcs = [1,-1,8,-8,-9,7,9,-7]

        var r: UInt64 = 0
        for direc in direcs {
            r |= getBitPuttables(color, direc: direc)
        }

        var ret: [(Int, Int)] = []
        for iy in 0..<height() {
            for ix in 0..<width() {
                let bitwhere: UInt64 = 1 << (UInt64(ix) + UInt64(iy) * 8)
                if r & bitwhere > 0 {
                    ret.append((ix, iy))
                }
            }
        }
        return ret
    }

    func getBitPuttables(color: Pieces, direc: Int) -> UInt64 {
        //        println(toString())
        //        println("\(direc)")
        var attacker: UInt64
        var attackee: UInt64

        var mask: UInt64

        //        println(bitBoardToString(black))
        //        println(bitBoardToString(white))

        switch direc {
            // Horizontal
        case 1:
            mask = 0x7e7e7e7e7e7e7e7e
        case -1:
            mask = 0x7e7e7e7e7e7e7e7e
            // Vertical
        case -8:
            mask = 0x00ffffffffffff00
        case 8:
            mask = 0x00ffffffffffff00
            // Diag LU -> RD
        case -9:
            mask = 0x7e7e7e7e7e7e7e7e
        case 7:
            mask = 0x7e7e7e7e7e7e7e7e
            // Diag RU -> LD
        case 9:
            mask = 0x7e7e7e7e7e7e7e7e
        case -7:
            mask = 0x7e7e7e7e7e7e7e7e
        default:
            assertionFailure("Should not reach this code!")
        }

        switch color {
        case .Black:
            attacker = black & mask
            attackee = white
        case .White:
            attacker = white & mask
            attackee = black
        default:
            assertionFailure("Should not reach this code!")
        }

        //        println(bitBoardToString(attacker))
        //        println(bitBoardToString(attackee))

        //        println(bitBoardToString(pos))
        var rev: UInt64 = 0
        var t: UInt64 = 0
        if direc >= 0 {
            t = attackee & (attacker >> UInt64(direc))
            t |= attackee & (t >> UInt64(direc))
            t |= attackee & (t >> UInt64(direc))
            t |= attackee & (t >> UInt64(direc))
            t |= attackee & (t >> UInt64(direc))
            t |= attackee & (t >> UInt64(direc))
            t = (t >> UInt64(direc))
        } else {
            t = attackee & (attacker << UInt64(-direc))
            t |= attackee & (t << UInt64(-direc))
            t |= attackee & (t << UInt64(-direc))
            t |= attackee & (t << UInt64(-direc))
            t |= attackee & (t << UInt64(-direc))
            t |= attackee & (t << UInt64(-direc))
            t = (t << UInt64(-direc))
        }

        var blank: UInt64 = ~(black | white)
        var ret = blank & t

        //        println(bitBoardToString(rev))
        
        return ret
    }

    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        let direcs = [1,-1,8,-8,-9,7,9,-7]

        var r: UInt64 = 0
        for direc in direcs {
            let pd = getBitReversible(color, x: x, y: y, direc: direc)
            r |= pd
        }

        var ret: [(Int, Int)] = []
        for iy in 0..<height() {
            for ix in 0..<width() {
                let bitwhere: UInt64 = 1 << (UInt64(ix) + UInt64(iy) * 8)
                if r & bitwhere > 0 {
                    ret.append((ix, iy))
                }
            }
        }
        return ret
    }

    func getBitReversible(color: Pieces, x: Int, y: Int, direc: Int) -> UInt64 {
//        println("getBitReversible \(color.toString()), \(x), \(y) for \(direc) - b:\(black) + w:\(white) :\n" + toString())
        var attacker: UInt64
        var attackee: UInt64

        var pp = 0
//        print("\(pp++)")
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
//print("\(pp++)")
        switch color {
        case .Black:
            attacker = black & mask
            attackee = white
        case .White:
            attacker = white & mask
            attackee = black
        default:
            assertionFailure("Should not reach this code!")
        }
//print("\(pp++)")
        var m1: UInt64
        var m2: UInt64
        var m3: UInt64
        var m4: UInt64
        var m5: UInt64
        var m6: UInt64
//print("\(pp++)")
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
        } else {
            ui64_direc = UInt64(-direc)
            m1 = pos << ui64_direc
            m2 = m1 << ui64_direc
            m3 = m2 << ui64_direc
            m4 = m3 << ui64_direc
            m5 = m4 << ui64_direc
            m6 = m5 << ui64_direc
        }
//print("\(pp++)")
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
                if ((m6 >> ui64_direc) & attacker) != 0 {
                    rev = m1 | m2 | m3 | m4 | m5 | m6
                }
            }
        }
//        println("getBitReversible \(color.toString()), \(x), \(y) for \(direc) - b:\(black) + w:\(white) :\n" + toString())
        return rev
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