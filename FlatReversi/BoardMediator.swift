//
//  BoardMediator.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class BoardMediator {
    private var board: Board

    // MARK: Initialization
    init(board:Board) {
        self.board = board
    }

    func initializeBoard() {
        // FIXME: This assumes 8x8
        board.initialize(8, height: 8)
        updateGuides(.Black)
    }

    // MARK: Direct access to Board

    func height() -> Int {
        return self.board.height()
    }

    func width() -> Int {
        return self.board.width()
    }

    func withinBoard(x: Int, y: Int) -> Bool {
        return self.board.withinBoard(x, y: y)
    }

    func set(color: Pieces, x: Int, y: Int) {
        self.board.set(color, x: x, y: y)
    }

    func get(x: Int, y: Int) -> Pieces {
        return self.board.get(x, y: y)
    }

    // MARK: Query functions

    func getNumBlack() -> Int {
        var ret = 0
        for sy in 0..<self.board.height() {
            for sx in 0..<self.board.width() {
                if(board.get(sx, y: sy) == Pieces.Black) {
                    ret++
                }
            }
        }

        return ret
    }

    func getNumWhite() -> Int {
        var ret = 0
        for sy in 0..<self.board.height() {
            for sx in 0..<self.board.width() {
                if(board.get(sx, y: sy) == Pieces.White) {
                    ret++
                }
            }
        }

        return ret
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        return (get(x, y: y) != .White && get(x, y: y) != .Black) && getReversible(color, x: x, y: y).count > 0;
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        var puttables: [(Int, Int)] = []
        for sy in 0..<self.board.height() {
            for sx in 0..<self.board.width() {
                if(canPut(color, x: sx, y: sy)) {
                    puttables.append((sx, sy))
                }
            }
        }

        return puttables
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        var reversed: [(Int, Int)] = []

        let directions = [
            (-1,-1), (0,-1), (1,-1),
            (-1,0),  /* */   (1,0),
            (-1,1),  (0,1),  (1,1),
        ]

        for direc in directions {
            var reversedForDirec: [(Int, Int)] = []
            var mode = 0
            for var d = 1; d < board.height(); ++d {
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

    // MARK: Board update functions

    // Does set and reverse pieces
    // Returns [x,y] of changes (except put piece)
    func put(color: Pieces, x: Int, y: Int, guides: Bool = true) -> [(Int, Int)] {
        if(withinBoard(x, y: y) && canPut(color, x: x, y: y)) {
            set(color, x: x, y: y)
            var reversed = reverse(color, x: x, y: y)
            if guides {
                updateGuides(nextTurn(color))
            }
            return reversed
        } else {
            return []
        }
    }

    func boardForAll(mapfun: (Pieces -> Pieces)) {
        for y in 0..<board.height() {
            for x in 0..<board.width() {
                var p = get(x, y: y)
                set(mapfun(p), x: x, y: y)
            }
        }
    }

    func boardForAll(mapfun: ((Int, Int) -> Pieces)) {
        for y in 0..<board.height() {
            for x in 0..<board.width() {
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

    // Normalize the board
    func reverse(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        var reversibles = getReversible(color, x: x, y: y)
        for (rev_x, rev_y) in reversibles {
            set(color, x: rev_x, y: rev_y)
        }

        return reversibles
    }

    func toString() -> String {
        return self.board.toString()
    }

    func clone() -> BoardMediator {
        let b = self.board.clone()
        let bm = BoardMediator(board:b)
        return bm
    }

    func nextTurn(color: Pieces) -> Pieces {
        var s : Pieces = .Black
        switch color {
        case .Black:
            s = .White
        case .White:
            s = .Black
        default:
            s = .Black
        }
        return s
    }
}