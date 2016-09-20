//
//  NegaAlphaSearch.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/21/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class NegaAlphaSearch : Search {
    var nodeCount = 0
    var startTimeInSec = 0.0
    var minTimeLimitInSec: Double = 5
    var timeLimitInSec: Double = 5
    var secPerDepth: Double = 1.2

    var totalDepth = 0

    let inf = 99999999.9

    func search(_ boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult {
        nodeCount = 0

        let date = Date()
        startTimeInSec = date.timeIntervalSince1970
        timeLimitInSec = max([minTimeLimitInSec, (Double(depth) * secPerDepth)])

        totalDepth = depth

        if let bitBoardClass = boardRepresentation.boardMediator.getBoard() as? FastBitBoard {
            let bitBoard = bitBoardClass.getUnsafeBitBoard()
            if let bitBoardEvaluator = evaluator as? BitBoardEvaluator {
                let (retVal, retPv) = recSearch(depth, board: bitBoard, forPlayer: forPlayer, currentPlayer: forPlayer, alpha: -inf, beta: inf, evaluator: bitBoardEvaluator)
                var ret = SearchResult(value: retVal, pv: [retPv])
                ret.nodesSearched = nodeCount
                ret.elapsedTimeInSec = Date().timeIntervalSince1970 - startTimeInSec
                return ret
            }
        }
        fatalError("TranspositionedAlphaBetaSearch needs bit board")
    }

    fileprivate func recSearch(_ depth: Int, board: BitBoard, forPlayer: Pieces, currentPlayer: Pieces, alpha: Double, beta: Double, evaluator: BitBoardEvaluator) -> (Double, (Int, Int)) {
        nodeCount += 1
        if depth <= 0 || board.isTerminal() {
            var eval = evaluator.evaluateBitBoard(board, forPlayer: forPlayer)
            if forPlayer != currentPlayer {eval = -eval}
            return (eval, (0,0))
        } else {
            var alpha = alpha
            var sign = -1.0
            var turn = currentPlayer
            var puttables = board.getPuttables(turn)

            if isEmpty(puttables) {
                // Pass and skip
                turn = nextTurn(turn)
                sign *= -1.0
                puttables = board.getPuttables(turn)
                if isEmpty(puttables) {
                    // Skip both sides. Termination
                    assertionFailure("Should not be reached at this code. Either of player should be able to play.")
                }
            }

            var bestValue = -inf

            var thisPv = (0,0)

            while !isEmpty(puttables) {
                let move = bitScanForward(puttables)
                puttables = xOrBitWhere(puttables, nthBit: move)

                let px = move % 8
                let py = move / 8

                var newBoard = board
                newBoard.put(turn, x: px, y: py, guides: false)

                var (val, _) = recSearch(depth-1, board: newBoard, forPlayer: forPlayer, currentPlayer: nextTurn(turn), alpha: -beta, beta: -alpha, evaluator: evaluator)
                val = sign * val

                if val > bestValue {
                    bestValue = val
                    thisPv = (px, py)
                }

                alpha = max(alpha, val)

                // cut
                if alpha >= beta {
                    break
                }
            }

            return (bestValue, thisPv)
        }
    }
}
