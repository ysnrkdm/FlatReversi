//
//  RandomPlayerWithEvaluation.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class RandomPlayerWithEvaluation: Player {

    var level: Int = 0
    var zones: Zones? = nil

    override func initialize(level: Int) {
        self.level = level
    }

    func configure(zones: Zones) {
        self.zones = zones
    }

    override func play() {
        dispatch_async_global({self.think()})
    }

    func think() {
        NSLog("Start thinking")
        var retx = 0
        var rety = 0

        if let puttables = playerMediator.getBoardRepresentative()?.getPuttables(self.color) {
            NSLog("puttables are \(puttables.count)")
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

        NSLog("Answer \(retx), \(rety)")
        playerMediator.put(self.color, x: retx, y: rety)
    }
}