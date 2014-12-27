//
//  ClassicalEvaluator.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/28/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class ClassicalEvaluator: BitBoardEvaluator {
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

    override func evaluate(boardRepresentation: BoardRepresentation, forPlayer: Pieces) -> Double {
        var bitBoard = BitBoard()

        for iy in 0..<bitBoard.height() {
            for ix in 0..<bitBoard.width() {
                bitBoard.set(boardRepresentation.get(ix, y: iy), x: ix, y: iy)
            }
        }

        return evaluateBitBoard(bitBoard, forPlayer: forPlayer)
        // Firstly looking into cache
//        if forPlayer == .Black {
//            if let value = boardEvalCacheBlack[boardHashFromTuple(boardRepresentation.hash())] {
//                NSLog("Black cache - \(boardEvalCacheBlack.count)")
//                return value
//            } else {
//                let value = eval(boardRepresentation, forPlayer: forPlayer)
//                boardEvalCacheBlack.updateValue(value, forKey: boardHashFromTuple(boardRepresentation.hash()))
//                return value
//            }
//        } else {
//            if let value = boardEvalCacheWhite[boardHashFromTuple(boardRepresentation.hash())] {
//                NSLog("White cache - \(boardEvalCacheWhite.count)")
//                return value
//            } else {
//                let value = eval(boardRepresentation, forPlayer: forPlayer)
//                boardEvalCacheWhite.updateValue(value, forKey: boardHashFromTuple(boardRepresentation.hash()))
//                return value
//            }
//        }
    }

    override func evaluateBitBoard(board: BitBoard, forPlayer: Pieces) -> Double {
        let ePossibleMoves = getWeightByPhase(wPossibleMoves, board: board) * Double(possibleMoves(board, forPlayer: forPlayer))
        let eEdge =
            getWeightByPhase(wEdge, board: board) * edge(board, forPlayer: forPlayer)
        let eFixedPieces =
            getWeightByPhase(wFixedPieces, board: board) * Double(fixedPieces(board, forPlayer: forPlayer))
        let eOpenness =
            getWeightByPhase(wOpenness, board: board) * Double(openness(board, forPlayer: forPlayer))
        let eBoardEvaluation =
            getWeightByPhase(wBoardEvaluation, board: board) * boardEvaluation(board, forPlayer: forPlayer, zones: zones)

//        println("\(ePossibleMoves), \(eEdge), \(eFixedPieces), \(eOpenness), \(eBoardEvaluation)")

        return ePossibleMoves + eEdge + eFixedPieces + eOpenness + eBoardEvaluation
    }

    // MARK: Factors

    func possibleMoves(board: BitBoard, forPlayer: Pieces) -> Int {
        return pop(board.getPuttables(forPlayer))
    }

    func edge(board: BitBoard, forPlayer: Pieces) -> Double {
        return 0.0
    }

    // Currently only count at edge
    func fixedPieces(board: BitBoard, forPlayer: Pieces) -> Int {
        let H = board.height()
        let W = board.width()

        let fixedPiecesEdgeHelper = { (board: BitBoard, forPlayer: Pieces, direc: ((Int, Int), (Int, Int), Int, (Int, Int))) -> Int in
            let org = direc.0
            let dst = direc.1
            let length = direc.2
            let direc = direc.3
            // No piece at either of corners
            if board.isEmpty(org.0, y: org.1) && board.isEmpty(dst.0, y: dst.1) {
                return 0
            }

            var cont = true
            var ret = 0
            var tmp = 0
            var cur = org
            for i in 1..<length-1 {
                cur = (org.0 + direc.0 * i, org.1 + direc.1 * i)
                if board.isEmpty(cur.0, y: cur.1) {
                    if cont {
                        cont = false
                        ret += tmp
                    }
                    tmp = 0
                }

                if board.get(cur.0, y: cur.1) == forPlayer {
                    ++tmp
                }
            }
            ret += tmp
            return ret
        }

        var ret = 0
        ret += fixedPiecesEdgeHelper(board, forPlayer, ((0,0), (W-1,0), W, (1,0)))
        ret += fixedPiecesEdgeHelper(board, forPlayer, ((0,0), (0,H-1), H, (0,1)))
        ret += fixedPiecesEdgeHelper(board, forPlayer, ((0,H-1), (W-1,H-1), W, (1,0)))
        ret += fixedPiecesEdgeHelper(board, forPlayer, ((W-1,0), (W-1,H-1), H, (0,1)))

        // Corner
        let fixedPiecesCornerHelper = { (board: BitBoard, forPlayer: Pieces, corner: (Int, Int)) -> Int in
        let (cx, cy) = corner
            var ret = 0
            if board.isPieceAt(forPlayer, x: cx, y: cy) {
                ++ret
            }
            return ret
        }

        ret += fixedPiecesCornerHelper(board, forPlayer, (0,0))
        ret += fixedPiecesCornerHelper(board, forPlayer, (W-1,0))
        ret += fixedPiecesCornerHelper(board, forPlayer, (0,H-1))
        ret += fixedPiecesCornerHelper(board, forPlayer, (W-1,H-1))

        return ret
    }

    func openness(board: BitBoard, forPlayer: Pieces) -> Int {
        var ret = 0
        var bb = board.getBoardForPlayer(forPlayer)

        while !isEmpty(bb) {
            let move = bitScanForward(bb)
            bb = xOrBitWhere(bb, move)

            let x = move % 8
            let y = move / 8

            ret += board.numPeripherals(.Empty, x: x, y: y)
        }

        return ret
    }

    func boardEvaluation(board: BitBoard, forPlayer: Pieces, zones: Zones) -> Double {
        var ret = 0.0
        let z = zones.zones
        var bb = board.getBoardForPlayer(forPlayer)

        while !isEmpty(bb) {
            let move = bitScanForward(bb)
            bb = xOrBitWhere(bb, move)

            let x = move % 8
            let y = move / 8

            ret += 1.0 * z[x][y]
        }
        
        return ret
    }

    // MARK: Private functions
    func getWeightByPhase(weight: [Double], board: BitBoard) -> Double {
        let perPhase: Int = 60 / weight.count
        var phase: Int = (60 - board.getNumVacant()) / perPhase
        phase = phase >= weight.count ? phase - 1 : phase
        return weight[phase]
    }
}