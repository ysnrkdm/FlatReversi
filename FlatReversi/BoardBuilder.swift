//
//  BoardBuilder.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/30/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class BoardBuilder {
    internal class func build(_ fromText: String) -> BoardRepresentation {
        let board = SimpleBitBoard()
        board.initialize(8, height: 8)

        var i = 0
        for c in fromText.characters {
            switch(c) {
            case "B":
                board.set(.black, x: i % 8, y: i / 8)
            case "W":
                board.set(.white, x: i % 8, y: i / 8)
            case ".":
                board.set(.empty, x: i % 8, y: i / 8)
            default:
                assertionFailure("")
            }
            i += 1
        }

        let bm = BoardMediator(board: board)
        let ret = BoardRepresentation(boardMediator: bm)
        return ret
    }

    internal class func buildBitBoard(_ fromText: String) -> BoardRepresentation {
        let board = SimpleBitBoard()
        board.initialize(8, height: 8)

        var i = 0
        for c in fromText.characters {
            switch(c) {
            case "B":
                board.set(.black, x: i % 8, y: i / 8)
            case "W":
                board.set(.white, x: i % 8, y: i / 8)
            case ".":
                board.set(.empty, x: i % 8, y: i / 8)
            default:
                assertionFailure("")
            }
            i += 1
        }

        let bm = BoardMediator(board: board)
        let ret = BoardRepresentation(boardMediator: bm)
        return ret
    }
}
