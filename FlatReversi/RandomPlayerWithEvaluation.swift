//
//  RandomPlayerWithEvaluation.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphite

class RandomPlayerWithEvaluation: ComputerPlayer {

    var zones: Zones? = nil

    func configure(_ zones: Zones) {
        self.zones = zones
    }

    override func think() {
        NSLog("Start thinking")
        var retx = 0
        var rety = 0

        if let puttables = playerMediator.getBoardRepresentation()?.getPuttables(self.color) {
            if puttables.count > 0 {
                if let uzones = zones {
                    NSLog("\n" + uzones.toString())
                    let coords = uzones.getTopNByRandomInPuttables(1, puttables: puttables)
                    if coords.count > 0 {
                        (retx, rety) = coords[0]
                    }
                }
            }
        } else {
            assertionFailure("Should not reach this code!")
        }

        playerMediator.put(self.color, x: retx, y: rety)
    }
}
