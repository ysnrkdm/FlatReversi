//
//  SimpleSearchTree.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/7/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class STNode {
    var value: Double = 0

    var expanded : Bool = false

    var whosTurn: Pieces = .None

    var parent: STNode?
    var children: [STNode] = []

    // MARK: Payloads
    var boardRepresentation: BoardRepresentation
    var put: (Int, Int) = (-1, -1)

    var depth : Int {
        var ret = 0
        var current = self
        while current.parent != nil {
            current = current.parent!
            ++ret
        }
        return ret
    }

    // MARK: Helper functions
    init(value: Double, whosTurn: Pieces, boardRepresentation: BoardRepresentation, parent: STNode?, children: [STNode] = []) {
        self.value = value

        self.whosTurn = whosTurn

        self.parent = parent
        self.children = children

        self.boardRepresentation = boardRepresentation.clone()
    }

    func toString() -> String {
        var ret = ""
        var stack = Stack<STNode>()
        stack.push(self)
        while !stack.isEmpty {
            if let current = stack.pop() {
                // Add children to stack
                var children = current.children
                // Sort by proofnum
                children.sort({$0.value < $1.value})

                for child in children {
                    stack.push(child)
                }

                // Do the job
                ret += current.nodeToString()
            }
        }

        return ret
    }

    func nodeToString() -> String {
        var spaces = ""
        for i in 0..<depth {
            if i == 0 && depth == 1 {
                spaces += "+"
            } else if i == depth - 1 {
                spaces += "+"
            } else {
                spaces += " "
            }
        }
        var ret = ""
        let showBoardRep = false
        ret = spaces + "\(whosTurn.toString()) \(value) : -- put \(put.0), \(put.1)\n"
        if showBoardRep {
            ret += "(boardRepresentation.toString())\n"
        }
        
        return ret
    }
}

class SearchResult {
    var value: Double
    var pv: [(Int, Int)]

    //
    var nodesSearched = 0
    var elapsedTimeInSec = 0.0
    var nps: Double {
        return Double(self.nodesSearched) / elapsedTimeInSec
    }

    init(value: Double, pv: [(Int, Int)]) {
        self.value = value
        self.pv = pv
    }

    func toString() -> String {
        return "\(value) - pv \(pv) : searched \(nodesSearched) nodes in \(elapsedTimeInSec). NPS is \(nps)"
    }
}

class SimpleSearchTree {
    var nodeCount = 0
    var startTimeInSec = 0.0
    var minTimeLimitInSec: Double = 5
    var timeLimitInSec: Double = 5
    var secPerDepth: Double = 1.2

    func search(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult {
        nodeCount = 0

        let date = NSDate()
        startTimeInSec = date.timeIntervalSince1970
        let inf = 99999999.9

        timeLimitInSec = max([minTimeLimitInSec, (Double(depth) * secPerDepth)])

        let ret = recSearch(depth, boardRepresentation: boardRepresentation, forPlayer: forPlayer, currentPlayer: forPlayer, alpha: -inf, beta: inf, evaluator: evaluator, pv: [])

        ret.nodesSearched = nodeCount
        ret.elapsedTimeInSec = NSDate().timeIntervalSince1970 - startTimeInSec
        return ret
    }

    func recSearch(depth: Int, boardRepresentation: BoardRepresentation, forPlayer: Pieces, currentPlayer: Pieces, alpha: Double, beta: Double, evaluator: Evaluator, pv: [(Int, Int)]) -> SearchResult {
        ++nodeCount
//        println("Searched \(nodeCount), depth = \(depth)")
        if depth <= 0 || boardRepresentation.isTerminal()/* || (NSDate().timeIntervalSince1970 - startTimeInSec) > timeLimitInSec*/ {
            // Depth reached, or terminal state. Returning
            let value = evaluator.eval(boardRepresentation, forPlayer: forPlayer)
            let ret = SearchResult(value: value, pv: pv)
//            println("\(ret.toString())\n\(boardRepresentation.toString())")

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

//            var value = forPlayer == turn ? -Double.infinity : Double.infinity

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