//
//  SearchEvalPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/23/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

//@_silgen_name("search")
//    func search(_: UInt64,_: UInt64,_: Int,_: Int) -> Int

class SearchEvalPlayer: ComputerPlayer {
    func configure(_ searcher: Search, zones: Zones, pnsLessThan: Int, searchDepth: Int, wPossibleMoves: [Double], wEdge: [Double], wFixedPieces: [Double], wOpenness: [Double], wBoardEvaluation: [Double]) {
        self.thinker = SearchEvalThink(searcher: searcher, zones: zones, pnsLessThan: pnsLessThan, searchDepth: searchDepth, wPossibleMoves: wPossibleMoves, wEdge: wEdge, wFixedPieces: wFixedPieces, wOpenness: wOpenness, wBoardEvaluation: wBoardEvaluation)
    }
}
