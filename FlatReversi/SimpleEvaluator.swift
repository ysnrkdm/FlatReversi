//
//  SimpleEvaluator.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/14/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class SimpleEvaluator: Evaluator {
    var wPossibleMoves: [Double] = [1.0]
    var wEdge: [Double] = [1.0]
    var wFixedPieces: [Double] = [1.0]
    var wOpenness: [Double] = [1.0]
    var wBoardEvaluation: [Double] = [1.0]

    var zones: Zones = Zones(width: 8, height: 8, initVal: 1)

    //    var boardEvalCacheBlack: Dictionary<BoardHash, Double> = Dictionary<BoardHash, Double>()
    //    var boardEvalCacheWhite: Dictionary<BoardHash, Double> = Dictionary<BoardHash, Double>()

    func configure(wPossibleMoves: [Double], wEdge: [Double], wFixedPieces: [Double], wOpenness: [Double], wBoardEvaluation: [Double], zones: Zones) {
        self.wPossibleMoves = wPossibleMoves
        self.wEdge = wEdge
        self.wFixedPieces = wFixedPieces
        self.wOpenness = wOpenness
        self.wBoardEvaluation = wBoardEvaluation
        self.zones = zones
    }

    func evaluate(boardRepresentation: BoardRepresentation, forPlayer: Pieces) -> Double {
        return eval(boardRepresentation, forPlayer: forPlayer)
    }

    func eval(board: BoardRepresentation, forPlayer: Pieces) -> Double {
        let ePossibleMoves = Double(arc4random()) / Double(UINT32_MAX)

        return ePossibleMoves
    }

    // MARK: Factors

    func possibleMoves(board: BoardRepresentation, forPlayer: Pieces) -> Int {
        return board.getPuttables(forPlayer).count
    }

    // MARK: Private functions
    func getWeightByPhase(weight: [Double], board: BoardRepresentation) -> Double {
        let perPhase: Int = 60 / weight.count
        var phase: Int = (60 - board.getNumVacant()) / perPhase
        phase = phase >= weight.count ? phase - 1 : phase
        return weight[phase]
    }
}