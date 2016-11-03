//
//  TreeSearchWithPerturbationPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/14/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class TreeSearchWithPerturbationPlayer: ComputerPlayer {
    func configure(_ zones: Zones, pnsLessThan: Int, searchDepth: Int, wPossibleMoves: [Double], wEdge: [Double], wFixedPieces: [Double], wOpenness: [Double], wBoardEvaluation: [Double], randomThreshold: Double) {
        self.thinker = TreeSearchWithPerturbationThink(zones: zones, pnsLessThan: pnsLessThan, searchDepth: searchDepth, wPossibleMoves: wPossibleMoves, wEdge: wEdge, wFixedPieces: wFixedPieces, wOpenness: wOpenness, wBoardEvaluation: wBoardEvaluation, randomThreshold: randomThreshold)
    }
}
