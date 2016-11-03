//
//  SimpleSearchStaticEvaluationPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class SimpleSearchStaticEvaluationPlayer: ComputerPlayer {
    func configure(_ zones: Zones, pnsLessThan: Int, searchDepth: Int, wPossibleMoves: [Double], wEdge: [Double], wFixedPieces: [Double], wOpenness: [Double], wBoardEvaluation: [Double]) {
        self.thinker = SimpleSearchStaticEvaluationThink(zones: zones, pnsLessThan: pnsLessThan, searchDepth: searchDepth, wPossibleMoves: wPossibleMoves, wEdge: wEdge, wFixedPieces: wFixedPieces, wOpenness: wOpenness, wBoardEvaluation: wBoardEvaluation)
    }
}
