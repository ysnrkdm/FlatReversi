//
//  SearchEvalPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/23/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

//@_silgen_name("search")
//    func search(_: UInt64,_: UInt64,_: Int,_: Int) -> Int

class SearchEvalPlayer: ComputerPlayer {
    var zones: Zones? = nil
    var pnsLessThan: Int = 0
    var searchDepth: Int = 1

    var wPossibleMoves: [Double] = [1.0]
    var wEdge: [Double] = [1.0]
    var wFixedPieces: [Double] = [1.0]
    var wOpenness: [Double] = [1.0]
    var wBoardEvaluation: [Double] = [1.0]

    var evaluator = ClassicalEvaluator()
    var searcher: Search = NegaAlphaSearch()

    func configure(searcher: Search, zones: Zones, pnsLessThan: Int, searchDepth: Int, wPossibleMoves: [Double], wEdge: [Double], wFixedPieces: [Double], wOpenness: [Double], wBoardEvaluation: [Double]) {
        self.searcher = searcher

        self.zones = zones
        self.pnsLessThan = pnsLessThan
        self.searchDepth = searchDepth

        self.wPossibleMoves = wPossibleMoves
        self.wEdge = wEdge
        self.wFixedPieces = wFixedPieces
        self.wOpenness = wOpenness
        self.wBoardEvaluation = wBoardEvaluation

        evaluator.configure(wPossibleMoves, wEdge: wEdge, wFixedPieces: wFixedPieces, wOpenness: wOpenness, wBoardEvaluation: wBoardEvaluation, zones: zones)
    }

    override func think() {
        NSLog("Start thinking")

        var retx = 0
        var rety = 0

        if let br = playerMediator.getBoardRepresentation() {
            if let bitBoardClass = br.boardMediator.getBoard() as? FastBitBoard {
                _ = bitBoardClass.getUnsafeBitBoard()
//                search(bitBoard.black, bitBoard.white, pnsLessThan, searchDepth)
            }
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
                NSLog("No PV found. Doing random.")
            }

            if puttables.count > 0 {
                let res = searcher.search(br.clone(), forPlayer: color, evaluator: evaluator, depth: searchDepth)

                NSLog("Searched -- " + res.toString())
                self.playerMediator.gameManager.setDebugString(res.toShortString())
                let coords = res.pv
                if coords.count > 0 {
                    (retx, rety) = coords[0]
                }
            }
        } else {
            assertionFailure("Should not reach this code!")
        }
        
        playerMediator.put(self.color, x: retx, y: rety)
    }
}