//
//  TranspositionedAlphaBetaSearch.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/12/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

struct BoardCacheTable {
    var turn: Pieces

    var lowerBound: Double
    var upperBound: Double
}

class TranspositionedAlphaBetaSearch : Search {
    var nodeCount = 0
    var transpositionHitCount = 0
    var startTimeInSec = 0.0
    var minTimeLimitInSec: Double = 5
    var timeLimitInSec: Double = 5
    var secPerDepth: Double = 1.2

    let inf: Double = 99999999

    var boardEvalCacheBlack: Dictionary<BitBoard, Double> = Dictionary<BitBoard, Double>()
    var boardEvalCacheWhite: Dictionary<BitBoard, Double> = Dictionary<BitBoard, Double>()

    var boardTransTable: Dictionary<BoardHash, BoardCacheTable> = Dictionary<BoardHash, BoardCacheTable>()

    func search(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult {
        nodeCount = 0

        let date = NSDate()
        startTimeInSec = date.timeIntervalSince1970

        timeLimitInSec = max([minTimeLimitInSec, (Double(depth) * secPerDepth)])

        boardTransTable = Dictionary<BoardHash, BoardCacheTable>()

        if let bitBoardClass = boardRepresentation.boardMediator.getBoard() as? FastBitBoard {
            let bitBoard = bitBoardClass.getUnsafeBitBoard()
            if let bitBoardEvaluator = evaluator as? BitBoardEvaluator {
                var ret = recSearch(depth, board: bitBoard, forPlayer: forPlayer, currentPlayer: forPlayer, alpha: -inf, beta: inf, evaluator: bitBoardEvaluator, pv: [])
                ret.nodesSearched = nodeCount
                ret.transpositionHitCount = transpositionHitCount
                ret.elapsedTimeInSec = NSDate().timeIntervalSince1970 - startTimeInSec
                return ret
            } else {
                assertionFailure("TranspositionedAlphaBetaSearch needs bit board")
            }
        } else {
            assertionFailure("TranspositionedAlphaBetaSearch needs bit board")
        }
    }

    func recSearch(depth: Int, board: BitBoard, forPlayer: Pieces, currentPlayer: Pieces, alpha: Double, beta: Double, evaluator: BitBoardEvaluator, pv: [(Int, Int)]) -> SearchResult {
        ++nodeCount
//        let bitBoard = board.getUnsafeBitBoard()
        var alpha = alpha
        var beta = beta

        if pv.count > 0 {
            if let trans = boardTransTable[board.hashValue] {
                if beta <= trans.lowerBound {
                    ++transpositionHitCount
                    return SearchResult(value: trans.lowerBound, pv: pv)
                } else if trans.upperBound <= alpha {
                    ++transpositionHitCount
                    return SearchResult(value: trans.upperBound, pv: pv)
                } else if trans.upperBound == trans.lowerBound {
                    ++transpositionHitCount
                    return SearchResult(value: trans.upperBound, pv: pv)
                }

                alpha = alpha < trans.lowerBound ? trans.lowerBound : alpha
                beta = trans.upperBound < beta ? trans.upperBound : beta
            }
        }

        if depth <= 0 || board.isTerminal() {
            // Depth reached, or terminal state. Returning
            let value = evaluator.evaluateBitBoard(board, forPlayer: forPlayer)
            let ret = SearchResult(value: value, pv: pv)
            let transToUpdate = BoardCacheTable(turn: currentPlayer, lowerBound: value, upperBound: value)
            boardTransTable.updateValue(transToUpdate, forKey: board.hashValue)
            return ret
        } else {
            // Here, no terminal state (at least one of players can play)

            var turn = currentPlayer
            // Generate Moves
            var puttables = board.getPuttables(turn)

            if isEmpty(puttables) {
                // Pass and skip
                turn = nextTurn(turn)
                puttables = board.getPuttables(turn)
                if isEmpty(puttables) {
                    // Skip both sides. Termination
                    assertionFailure("Should not be reached at this code. Either of player should be able to play.")
                }
            }

            var state: SearchResult = SearchResult(value: 0, pv: pv)

            while !isEmpty(puttables) {
                var newBoard = board
                let move = bitScanForward(puttables)
                puttables = xOrBitWhere(puttables, move)
                let px = move % 8
                let py = move / 8
                newBoard.put(turn, x: px, y: py, guides: false)
                var newPv = pv
                newPv.append((px, py))
                let r = recSearch(depth - 1, board: newBoard, forPlayer: forPlayer, currentPlayer: nextTurn(turn), alpha: alpha, beta: beta, evaluator: evaluator, pv: newPv)

                if forPlayer == turn {
                    // fail high
                    if beta <= r.value {
                        var upperBound = inf
                        if let trans = boardTransTable[board.hashValue] {
                            upperBound = trans.upperBound
                        }
                        let transToUpdate = BoardCacheTable(turn: turn, lowerBound: r.value, upperBound: upperBound)
                        boardTransTable.updateValue(transToUpdate, forKey: board.hashValue)
                        state = r
                        break
                    }
                    // Max
                    if alpha < r.value {
                        state = r
                        alpha = r.value
                    }

                    if alpha >= beta {
                        // beta cut
                        break
                    }
                } else {
                    // fail low
                    if r.value <= alpha {
                        var lowerBound = -inf
                        if let trans = boardTransTable[board.hashValue] {
                            lowerBound = trans.lowerBound
                        }
                        let transToUpdate = BoardCacheTable(turn: turn, lowerBound: lowerBound, upperBound: r.value)
                        boardTransTable.updateValue(transToUpdate, forKey: board.hashValue)
                        state = r
                        break
                    }
                    // Min
                    if beta > r.value {
                        state = r
                        beta = r.value
                    }

                    if alpha >= beta {
                        // alpha cut
                        break
                    }
                }
            }
            
            return state
        }
    }
}