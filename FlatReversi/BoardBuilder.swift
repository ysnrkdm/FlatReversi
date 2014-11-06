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
        var board = Board()
        board.initialize(8, height: 8)

        var i = 0
        for c in fromText {
            switch(c) {
            case "B":
                board.set(.Black, x: i / 8, y: i % 8)
            case "W":
                board.set(.White, x: i / 8, y: i % 8)
            case ".":
                //board.set(.Black, i / 8, i % 8)
                break
            default:
                assertionFailure("")
            }
            ++i
        }

        let bm = BoardMediator()
        bm.board = board
        let ret = BoardRepresentation(boardMediator: bm)
        return ret
    }
}