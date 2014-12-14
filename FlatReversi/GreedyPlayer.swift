//
//  GreedyPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/27/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class GreedyPlayer: ComputerPlayer {
    override func think() {
        NSLog("Start thinking")
        var retx = 0
        var rety = 0

        if let puttables = playerMediator.getBoardRepresentation()?.getPuttables(self.color) {
            var maxNumReversibles = 0
            if puttables.count > 0 {
                for (px, py) in puttables {
                    if let reversible = self.playerMediator.getBoardRepresentation()?.getReversible(self.color, x: px, y: py) {
                        if maxNumReversibles < reversible.count {
                            (retx, rety) = (px, py)
                            maxNumReversibles = reversible.count
                        }
                    }
                }
            }
        } else {
            assertionFailure("Should not reach this code!")
        }

        playerMediator.put(self.color, x: retx, y: rety)
    }
}