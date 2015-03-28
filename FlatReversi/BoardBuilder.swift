//
//  BoardBuilder.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/30/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class BoardBuilder {
    internal class func build(fromText: String) -> BoardRepresentation {
        var board = SimpleBitBoard()
        board.initialize(8, height: 8)

        var i = 0
        for c in fromText {
            switch(c) {
            case "B":
                board.set(.Black, x: i % 8, y: i / 8)
            case "W":
                board.set(.White, x: i % 8, y: i / 8)
            case ".":
                board.set(.Empty, x: i % 8, y: i / 8)
            default:
                assertionFailure("")
            }
            ++i
        }

        let bm = BoardMediator(board: board)
        let ret = BoardRepresentation(boardMediator: bm)
        return ret
    }

    internal class func buildBitBoard(fromText: String) -> BoardRepresentation {
        var board = SimpleBitBoard()
        board.initialize(8, height: 8)

        var i = 0
        for c in fromText {
            switch(c) {
            case "B":
                board.set(.Black, x: i % 8, y: i / 8)
            case "W":
                board.set(.White, x: i % 8, y: i / 8)
            case ".":
                board.set(.Empty, x: i % 8, y: i / 8)
            default:
                assertionFailure("")
            }
            ++i
        }

        let bm = BoardMediator(board: board)
        let ret = BoardRepresentation(boardMediator: bm)
        return ret
    }
}