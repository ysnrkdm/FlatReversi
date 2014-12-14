//
//  ArrayBoard.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/18/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class ArrayBoard: Board {
    var board : [[Pieces]] = []
    var _height = 8
    var _width = 8
    func height() -> Int {
        return _height
    }
    func width() -> Int {
        return _width
    }

    func initialize(width: Int, height: Int) {
        self._height = height
        self._width = width

        for y in 0..<height {
            var row: [Pieces] = []
            for w in 0..<width { row.append(.Empty) }
            board.append(row)
            for x in 0..<width {
                board[y][x] = .Empty
                // Put White/Black pieces at the middle of the board
                if((x == 3 || x == 4) && (y == 3 || y == 4)) {
                    if(x == y) {
                        board[y][x] = .Black
                    } else {
                        board[y][x] = .White
                    }
                }
            }
        }
    }

    // MARK: Basic functions
    func withinBoard(x: Int, y: Int) -> Bool {
        return (0 <= x && x < width() && 0 <= y && y < height())
    }

    func set(color: Pieces, x: Int, y: Int) {
        if(withinBoard(x, y: y)) {
            board[y][x] = color
        }
    }

    func get(x: Int, y: Int) -> Pieces {
        if(withinBoard(x, y: y)) {
            return board[y][x]
        } else {
            return .None
        }
    }

    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool {
        return get(x, y: y) == piece
    }

    // MARK: Query functions
    func getNumBlack() -> Int {
        var ret = 0
        for sy in 0..<height() {
            for sx in 0..<width() {
                if(get(sx, y: sy) == Pieces.Black) {
                    ret++
                }
            }
        }

        return ret
    }

    func getNumWhite() -> Int {
        var ret = 0
        for sy in 0..<height() {
            for sx in 0..<width() {
                if(get(sx, y: sy) == Pieces.White) {
                    ret++
                }
            }
        }

        return ret
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        return (get(x, y: y) != .White && get(x, y: y) != .Black) && getReversible(color, x: x, y: y).count > 0
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        var puttables: [(Int, Int)] = []
        for sy in 0..<height() {
            for sx in 0..<width() {
                if(canPut(color, x: sx, y: sy)) {
                    puttables.append((sx, sy))
                }
            }
        }

        return puttables
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
//        println(toString())
        var reversed: [(Int, Int)] = []

        let directions = [
            (-1,-1), (0,-1), (1,-1),
            (-1,0),  /* */   (1,0),
            (-1,1),  (0,1),  (1,1),
        ]

        for direc in directions {
            var reversedForDirec: [(Int, Int)] = []
            var mode = 0
            for var d = 1; d < height(); ++d {
                var cx = direc.0 * d + x
                var cy = direc.1 * d + y
                if(!withinBoard(cx, y: cy)) {
                    break
                }
                var p = get(cx, y: cy)

                if((mode == 0 || mode == 1) && p != color && p != .Empty && p != .Guide && p != .None) {
                    mode = 1
                    let coordToAdd = (cx, cy)
                    reversedForDirec.append(coordToAdd)
                } else if (mode == 1 && p == color) {
                    // Can reverse them!
                    reversed += reversedForDirec
                } else {
                    break
                }
            }
        }
        
        return reversed
    }

    func isEmpty(x: Int, y: Int) -> Bool {
        let p = get(x, y: y)
        return p != .Black && p != .White && p != .None
    }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        let peripherals = [
            (-1, -1), (0,-1), (1,-1),
            (-1,  0),         (1, 0),
            (-1,  1), (0, 1), (1, 1),
        ]

        var ret = 0
        for (dx, dy) in peripherals {
            if color == .Black {
                if get(x + dx, y: y + dy) == .Black {
                    ++ret
                }
            } else if color == .White {
                if get(x + dx, y: y + dy) == .Black {
                    ++ret
                }
            } else if color == .Empty {
                if isEmpty(x + dx, y: y + dy) {
                    ++ret
                }
            }
        }

        return ret
    }

    func hash() -> (UInt64, UInt64) {
        return (0,0)
    }

    // MARK: Update functions

    // MARK: Utility functions
    func clone() -> Board {
        let b: [[Pieces]] = self.board
        var ret = ArrayBoard()
        ret.board = b
        ret._height = self.height()
        ret._width = self.width()

        return ret
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