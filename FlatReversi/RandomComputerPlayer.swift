//
//  RandomComputerPlayer.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class RandomComputerPlayer: ComputerPlayer {

    override func think() {
        NSLog("Start thinking")
        var retx = 0
        var rety = 0
        if(level >= 1) {
            if let puttables = playerMediator.getBoardRepresentation()?.getPuttables(self.color) {
                if(puttables.count > 0) {
                    let selected = Int(arc4random_uniform((puttables.count - 1) - 0 + 1)) + 0
                    assert(selected < puttables.count, "selected must be less than num of puttables")
                    (retx, rety) = puttables[selected]
                }
            } else {
                assertionFailure("Should not reach this code!")
            }
        } else {

        }

        playerMediator.put(self.color, x: retx, y: rety)
    }
}