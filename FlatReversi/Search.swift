//
//  Search.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/12/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

//class STNode {
//    var value: Double = 0
//
//    var expanded : Bool = false
//
//    var whosTurn: Pieces = .None
//
//    var parent: STNode?
//    var children: [STNode] = []
//
//    // MARK: Payloads
//    var boardRepresentation: BoardRepresentation
//    var put: (Int, Int) = (-1, -1)
//
//    var depth : Int {
//        var ret = 0
//        var current = self
//        while current.parent != nil {
//            current = current.parent!
//            ++ret
//        }
//        return ret
//    }
//
//    // MARK: Helper functions
//    init(value: Double, whosTurn: Pieces, boardRepresentation: BoardRepresentation, parent: STNode?, children: [STNode] = []) {
//        self.value = value
//
//        self.whosTurn = whosTurn
//
//        self.parent = parent
//        self.children = children
//
//        self.boardRepresentation = boardRepresentation.clone()
//    }
//
//    func toString() -> String {
//        var ret = ""
//        var stack = Stack<STNode>()
//        stack.push(self)
//        while !stack.isEmpty {
//            if let current = stack.pop() {
//                // Add children to stack
//                var children = current.children
//                // Sort by proofnum
//                children.sort({$0.value < $1.value})
//
//                for child in children {
//                    stack.push(child)
//                }
//
//                // Do the job
//                ret += current.nodeToString()
//            }
//        }
//
//        return ret
//    }
//
//    func nodeToString() -> String {
//        var spaces = ""
//        for i in 0..<depth {
//            if i == 0 && depth == 1 {
//                spaces += "+"
//            } else if i == depth - 1 {
//                spaces += "+"
//            } else {
//                spaces += " "
//            }
//        }
//        var ret = ""
//        let showBoardRep = false
//        ret = spaces + "\(whosTurn.toString()) \(value) : -- put \(put.0), \(put.1)\n"
//        if showBoardRep {
//            ret += "(boardRepresentation.toString())\n"
//        }
//
//        return ret
//    }
//}

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

struct BoardHash : Hashable, Equatable {
    var x: UInt64
    var y: UInt64

    var hashValue: Int { return Int(x) ^ Int(y) }
}

func boardHashFromTuple(tuple: (UInt64, UInt64)) -> BoardHash {
    return BoardHash(x: tuple.0, y: tuple.1)
}

struct BoardCacheTable {
    var turn: Pieces

    var lowerBound: Double
    var upperBound: Double

    var next: (Int, Int)
}

func == (lhs: BoardHash, rhs: BoardHash) -> Bool {
    return lhs.x == rhs.x && lhs.y == rhs.y
}

protocol Search {
    func search(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult
    func evaluate(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator) -> Double
}