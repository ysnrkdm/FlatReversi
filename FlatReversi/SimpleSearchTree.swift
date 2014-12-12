//
//  SimpleSearchTree.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/7/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class SimpleSearchTree : Search {
    var nodeCount = 0
    var startTimeInSec = 0.0
    var minTimeLimitInSec: Double = 5
    var timeLimitInSec: Double = 5
    var secPerDepth: Double = 1.2

    let inf = 99999999.9

    var boardEvalCacheBlack: Dictionary<BoardHash, Double> = Dictionary<BoardHash, Double>()
    var boardEvalCacheWhite: Dictionary<BoardHash, Double> = Dictionary<BoardHash, Double>()

    func search(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult {
        nodeCount = 0

        let date = NSDate()
        startTimeInSec = date.timeIntervalSince1970

        timeLimitInSec = max([minTimeLimitInSec, (Double(depth) * secPerDepth)])

        let ret = recSearch(depth, boardRepresentation: boardRepresentation, forPlayer: forPlayer, currentPlayer: forPlayer, alpha: -inf, beta: inf, evaluator: evaluator, pv: [])

        ret.nodesSearched = nodeCount
        ret.elapsedTimeInSec = NSDate().timeIntervalSince1970 - startTimeInSec
        return ret
    }

    func evaluate(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator) -> Double {
        // Firstly looking into cache
        if forPlayer == .Black {
            if let value = boardEvalCacheBlack[boardHashFromTuple(boardRepresentation.hash())] {
                return value
            } else {
                let value = evaluator.eval(boardRepresentation, forPlayer: forPlayer)
                boardEvalCacheBlack.updateValue(value, forKey: boardHashFromTuple(boardRepresentation.hash()))
                return value
            }
        } else {
            if let value = boardEvalCacheWhite[boardHashFromTuple(boardRepresentation.hash())] {
                return value
            } else {
                let value = evaluator.eval(boardRepresentation, forPlayer: forPlayer)
                boardEvalCacheWhite.updateValue(value, forKey: boardHashFromTuple(boardRepresentation.hash()))
                return value
            }
        }
    }

    func recSearch(depth: Int, boardRepresentation: BoardRepresentation, forPlayer: Pieces, currentPlayer: Pieces, alpha: Double, beta: Double, evaluator: Evaluator, pv: [(Int, Int)]) -> SearchResult {
        ++nodeCount
        if depth <= 0 || boardRepresentation.isTerminal() {
            // Depth reached, or terminal state. Returning
            let value = evaluate(boardRepresentation, forPlayer: forPlayer, evaluator: evaluator)
            let ret = SearchResult(value: value, pv: pv)

            return ret
        } else {
            // Here, no terminal state (at least one of players can play)

            var turn = currentPlayer
            // Generate Moves
            var puttables = boardRepresentation.getPuttables(turn)

            if puttables.isEmpty {
                // Pass and skip
                turn = boardRepresentation.boardMediator.nextTurn(turn)
                puttables = boardRepresentation.getPuttables(turn)
                if puttables.isEmpty {
                    // Skip both sides. Termination
                    assertionFailure("Should not be reached at this code. Either of player should be able to play.")
                }
            }

            var state: SearchResult = SearchResult(value: 0, pv: pv)
            var alpha = alpha
            var beta = beta

            for (px, py) in puttables {
                var newBoard = boardRepresentation.clone()
                newBoard.boardMediator.put(turn, x: px, y: py, guides: false)
                var newPv = pv
                newPv.append((px, py))
                let r = recSearch(depth - 1, boardRepresentation: newBoard, forPlayer: forPlayer, currentPlayer: boardRepresentation.boardMediator.nextTurn(turn), alpha: alpha, beta: beta, evaluator: evaluator, pv: newPv)

                if forPlayer == turn {
                    // Max
                    if alpha < r.value {
                        state = r
                        alpha = r.value
                    }
                } else {
                    // Min
                    if beta > r.value {
                        state = r
                        beta = r.value
                    }
                }
                if alpha >= beta {
                    break
                }
            }

            return state
        }
    }
}