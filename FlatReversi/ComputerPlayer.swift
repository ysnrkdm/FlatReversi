//
//  ComputerPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/27/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class ComputerPlayer: Player {
    var level: Int = 0
    var info: Info = SimpleLogInfo()
    var thinker: Think? = nil

    override func initialize(_ level: Int) {
        self.level = level
    }

    func configure() {
        NSLog("Define configure method with arguments.")
    }

    override func play() {
        dispatch_async_global({self.thinkAndPlay()})
    }

    func thinkAndPlay() {
        if let board = playerMediator.getBoardRepresentation(), let thinker = self.thinker {
            let hand = think(board: board, thinker: thinker)
            playerMediator.put(self.color, x: hand.col, y: hand.row)
        } else {
            assertionFailure("Should not reach this code!")
        }
    }
    
    func think(board: BoardRepresentation, thinker: Think) -> Hand {
        return thinker.think(self.color, board: board, info: self.info)
    }
}
