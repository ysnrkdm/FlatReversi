//
//  BoardRepresentation.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class BoardRepresentation {
    var boardMediator: BoardMediator

    init(boardMediator: BoardMediator) {
        self.boardMediator = boardMediator
    }

    func height() -> Int {
        return self.boardMediator.height()
    }

    func width() -> Int {
        return self.boardMediator.width()
    }

    func withinBoard(x: Int, y: Int) -> Bool {
        return self.boardMediator.withinBoard(x, y: y)
    }

    func get(x: Int, y: Int) -> Pieces {
        return self.boardMediator.get(x, y: y)
    }

    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool {
        return self.boardMediator.isPieceAt(piece, x: x, y: y)
    }

    func isEmpty(x: Int, y: Int) -> Bool {
        return self.boardMediator.isEmpty(x, y: y)
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        return (get(x, y: y) != .White && get(x, y: y) != .Black) && getReversible(color, x: x, y: y).count > 0;
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        return self.boardMediator.getPuttables(color)
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        return boardMediator.getReversible(color, x: x, y: y)
    }

    func getNumBlack() -> Int {
        return boardMediator.getNumBlack()
    }

    func getNumWhite() -> Int {
        return boardMediator.getNumWhite()
    }

    func getNumVacant() -> Int {
        return 64 - getNumBlack() - getNumWhite()
    }

    func isTerminal() -> Bool {
        if getNumVacant() == 0 {
            return true
        }

        if getPuttables(.Black).isEmpty && getPuttables(.White).isEmpty {
            return true
        }

        return false
    }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int {
        return self.boardMediator.numPeripherals(color, x: x, y: y)
    }

    func hash() -> (UInt64, UInt64) {
        return self.boardMediator.hash()
    }

    func toString() -> String {
        return self.boardMediator.toString()
    }

    func clone() -> BoardRepresentation {
        let bm = self.boardMediator.clone()
        let newBR = BoardRepresentation(boardMediator: bm)

        return newBR
    }
}