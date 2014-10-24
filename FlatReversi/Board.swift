//
//  Board.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum Pieces {
    case Black, White, Empty, Guide, None

    func toString() -> String {
        switch self {
        case .Black:
            return "B"
        case .White:
            return "W"
        case .Empty:
            return "."
        case .Guide:
            return "@"
        case .None:
            return "*"
        default:
            return "!"
        }
    }
}

class Board {
    var board : [[Pieces]] = []
    var _height = 8
    var _width = 8
    var height: Int {
        return _height
    }
    var width: Int {
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

    func withinBoard(x: Int, y: Int) -> Bool {
        return (0 <= x && x < width && 0 <= y && y < height)
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

    func toString() -> String {
        var ret = ""
        for var y = 0; y < self.height; ++y {
            for var x = 0; x < self.width; ++x {
                var p = get(x, y: y)
                var s = p.toString()
                ret += " " + s + " "
            }
            ret += "\n"
        }

        return ret
    }
}