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

    func isEmpty(x: Int, y: Int) -> Bool {
        let p = get(x, y: y)
        return p != .Black && p != .White
    }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool {
        return (get(x, y: y) != .White && get(x, y: y) != .Black) && getReversible(color, x: x, y: y).count > 0;
    }

    func getPuttables(color: Pieces) -> [(Int, Int)] {
        return self.boardMediator.getPuttables(color)
    }

    // Only diag or horizontal/vertical lines can change by putting piece at x,y
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] {
        var reversed: [(Int, Int)] = []

        let directions = [
            [-1,-1], [0,-1], [1,-1],
            [-1,0],  /* */   [1,0],
            [-1,1],  [0,1],  [1,1],
        ]

        for direc in directions {
            var reversedForDirec: [(Int, Int)] = []
            var mode = 0
            for var d = 1; d < boardMediator.height(); ++d {
                var cx = direc[0] * d + x
                var cy = direc[1] * d + y
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
                    for reversible in reversedForDirec {
                        reversed.append(reversible)
                    }
                } else {
                    break
                }
            }
        }
        
        return reversed
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

    func toString() -> String {
        return self.boardMediator.toString()
    }

    func clone() -> BoardRepresentation {
        let bm = self.boardMediator.clone()
        let newBR = BoardRepresentation(boardMediator: bm)

        return newBR
    }
}