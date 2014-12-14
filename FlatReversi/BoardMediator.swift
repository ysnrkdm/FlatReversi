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

    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool {
        return self.board.isPieceAt(piece, x: x, y: y)
    }

    // MARK: Query functions
    func getNumBlack() -> Int {
        return self.board.getNumBlack()
    }

    func getNumWhite() -> Int {
        return self.board.getNumWhite()
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        return self.board.canPut(color, x: x, y: y)
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        return self.board.getPuttables(color)
    }

    func isEmpty(x: Int, y: Int) -> Bool {
        return self.board.isEmpty(x, y: y)
    }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        return self.board.numPeripherals(color, x: x, y: y)
    }

    func hash() -> (UInt64, UInt64) {
        return self.board.hash()
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        return self.board.getReversible(color, x: x, y: y)
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