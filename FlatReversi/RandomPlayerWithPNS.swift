//
//  RandomPlayerWithPNS.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/6/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class RandomPlayerWithPNS: ComputerPlayer {

    var zones: Zones? = nil
    var pnsLessThan: Int = 0

    func configure(zones: Zones, pnsLessThan: Int) {
        self.zones = zones
        self.pnsLessThan = pnsLessThan
    }

    override func think() {
        NSLog("Start thinking")
        var retx = 0
        var rety = 0

        if let br = playerMediator.getBoardRepresentation() {
            let puttables = br.getPuttables(color)

            if br.getNumVacant() < pnsLessThan {
                let solver = SimpleProofSolver()
                let answer = solver.solve(br.clone(), forPlayer: color)
                NSLog("Solving by PNS search...")
                if ((answer.proof == .BlackWin && color == .Black) || (answer.proof == .WhiteWin && color == .White)) && answer.moves.count > 0 {
                    (retx, rety) = answer.moves[0]
                    NSLog("Found PV! Answer is \(retx), \(rety)")
                    playerMediator.put(self.color, x: retx, y: rety)
                    return
                }
            }

            NSLog("No PV found. Doing random.")
            if puttables.count > 0 {
                if let uzones = zones {
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