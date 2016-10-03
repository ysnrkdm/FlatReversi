//
//  BoardRepresentation.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

public class BoardRepresentation {
    public var boardMediator: BoardMediator

    public init(boardMediator: BoardMediator) {
        self.boardMediator = boardMediator
    }

    public func height() -> Int {
        return self.boardMediator.height()
    }

    public func width() -> Int {
        return self.boardMediator.width()
    }

    public func withinBoard(_ x: Int, y: Int) -> Bool {
        return self.boardMediator.withinBoard(x, y: y)
    }

    public func get(_ x: Int, y: Int) -> Pieces {
        return self.boardMediator.get(x, y: y)
    }

    public func isPieceAt(_ piece: Pieces, x: Int, y: Int) -> Bool {
        return self.boardMediator.isPieceAt(piece, x: x, y: y)
    }

    public func isEmpty(_ x: Int, y: Int) -> Bool {
        return self.boardMediator.isEmpty(x, y: y)
    }

    public func canPut(_ color: Pieces, x: Int, y: Int) -> Bool {
        return (get(x, y: y) != .white && get(x, y: y) != .black) && getReversible(color, x: x, y: y).count > 0;
    }

    public func getPuttables(_ color: Pieces) -> [(Int, Int)] {
        return self.boardMediator.getPuttables(color)
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    public func getReversible(_ color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        return boardMediator.getReversible(color, x: x, y: y)
    }

    public func isAnyPuttable(_ color: Pieces) -> Bool {
        return boardMediator.isAnyPuttable(color)
    }

    public func getNumBlack() -> Int {
        return boardMediator.getNumBlack()
    }

    public func getNumWhite() -> Int {
        return boardMediator.getNumWhite()
    }

    public func getNumVacant() -> Int {
        return 64 - getNumBlack() - getNumWhite()
    }

    public func isTerminal() -> Bool {
        if getNumVacant() == 0 {
            return true
        }

        if isAnyPuttable(.black) {
            return false
        }

        if isAnyPuttable(.white) {
            return false
        }

        return true
    }

    public func numPeripherals(_ color: Pieces, x: Int, y: Int) -> Int {
        return self.boardMediator.numPeripherals(color, x: x, y: y)
    }

    public func hashValue() -> Int {
        return self.boardMediator.hashValue()
    }

    public func toString() -> String {
        return self.boardMediator.toString()
    }

    public func clone() -> BoardRepresentation {
        let bm = self.boardMediator.clone()
        let newBR = BoardRepresentation(boardMediator: bm)

        return newBR
    }
}
