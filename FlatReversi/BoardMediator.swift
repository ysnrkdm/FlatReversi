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

    func isAnyPuttable(color: Pieces) -> Bool {
        return self.board.isAnyPuttable(color)
    }

    func isEmpty(x: Int, y: Int) -> Bool {
        return self.board.isEmpty(x, y: y)
    }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        return self.board.numPeripherals(color, x: x, y: y)
    }

    func hashValue() -> Int {
        return self.board.hashValue()
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        return self.board.getReversible(color, x: x, y: y)
    }

    // MARK: Board update functions

    func updateGuides(color: Pieces) -> Int {
        return self.board.updateGuides(color)
    }

    // Does set and reverse pieces
    // Returns [x,y] of changes (except put piece)
    func put(color: Pieces, x: Int, y: Int, guides: Bool = true, returnChanges: Bool = true) -> [(Int, Int)] {
        return self.board.put(color, x: x, y: y, guides: guides, returnChanges: returnChanges)
    }

    // Normalize the board

    func toString() -> String {
        return self.board.toString()
    }

    func clone() -> BoardMediator {
        let b = self.board.clone()
        let bm = BoardMediator(board:b)
        return bm
    }

    func getBoard() -> Board {
        return self.board
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