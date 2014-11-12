//
//  ClassicalEvaluator.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/28/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class ClassicalEvaluator: Evaluator {
    var wPossibleMoves: [Double] = [1.0]
    var wEdge: [Double] = [1.0]
    var wFixedPieces: [Double] = [1.0]
    var wOpenness: [Double] = [1.0]
    var wBoardEvaluation: [Double] = [1.0]

    var zones: Zones = Zones(width: 8, height: 8, initVal: 1)

    func configure(wPossibleMoves: [Double], wEdge: [Double], wFixedPieces: [Double], wOpenness: [Double], wBoardEvaluation: [Double], zones: Zones) {
        self.wPossibleMoves = wPossibleMoves
        self.wEdge = wEdge
        self.wFixedPieces = wFixedPieces
        self.wOpenness = wOpenness
        self.wBoardEvaluation = wBoardEvaluation
        self.zones = zones
    }

    func eval(board: BoardRepresentation, forPlayer: Pieces) -> Double {
        return 0.0
    }

    // MARK: Factors

    func possibleMoves(board: BoardRepresentation, forPlayer: Pieces) -> Int {
        return board.getPuttables(forPlayer).count
    }

    func edge(board: BoardRepresentation, forPlayer: Pieces) -> Double {
        return 0.0
    }

    // Currently only count at edge
    func fixedPieces(board: BoardRepresentation, forPlayer: Pieces) -> Int {
        let H = board.height()
        let W = board.width()
        let direcs = [
            ((0,0), (W-1,0), W, (1,0)),
            ((0,0), (0,H-1), H, (0,1)),
            ((0,H-1), (W-1,H-1), W, (1,0)),
            ((W-1,0), (W-1,H-1), H, (0,1)),
        ]

        var ret = 0
        for d in direcs {
            let org = d.0
            let dst = d.1
            let length = d.2
            let direc = d.3
            // No piece at either of corners
            if board.isEmpty(org.0, y: org.1) && board.isEmpty(dst.0, y: dst.1) {
                continue
            }

            var cont = true
            var tmp = 0
            var cur = org
            for i in 1..<length-1 {
                cur = (org.0 + direc.0 * i, org.1 + direc.1 * i)
                if board.isEmpty(cur.0, y: cur.1) {
                    cont = false
                }

                if board.get(cur.0, y: cur.1) == forPlayer {
                    ++tmp
                }

                if !cont {
                    ret += tmp
                    tmp = 0
                    break
                }

                if (cur.0 == dst.0 && cur.1 == dst.1) {

                }
            }
        }

        return ret
    }

    func openness(board: BoardRepresentation, forPlayer: Pieces) -> Int {
        return 0
    }

    func boardEvaluation(board: BoardRepresentation, forPlayer: Pieces) -> Double {
        return 0.0
    }

    // MARK: Private functions
    func getWeightByPhase(weight: [Double], board: BoardRepresentation) -> Double {
        let perPhase: Int = 60 / weight.count
        var phase: Int = (60 - board.getNumVacant()) / perPhase
        phase = phase >= weight.count ? phase - 1 : phase
        return weight[phase]
    }
}