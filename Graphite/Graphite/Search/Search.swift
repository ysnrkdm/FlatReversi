//
//  Search.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/12/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

public struct SearchResult {
    var value: Double = 0
    public var pv: [(Int, Int)] = []

    //
    var nodesSearched = 0
    var elapsedTimeInSec = 0.0
    var transpositionHitCount = 0

    init(value: Double) {
        self.value = value
    }

    public init(value: Double, pv: [(Int, Int)]) {
        self.value = value
        self.pv = pv
    }

    public var nps: Double {
        return Double(self.nodesSearched) / elapsedTimeInSec
    }

    public func toString() -> String {
        let transRatio: Double = Double(transpositionHitCount) / Double(nodesSearched) * 100.0
        return "\(value) - pv \(pv) : searched \(nodesSearched) (\(transRatio)% transposition table hit) nodes in \(elapsedTimeInSec). NPS is \(nps)"
    }

    public func toShortString() -> String {
        let eltime = String(format: "%.01f", Float(elapsedTimeInSec))
        let npsStr = String(format: "%.01f", Float(nps))

        return "\(nodesSearched) nodes in \(eltime) secs. NPS: \(npsStr)"

    }
}

typealias BoardHash = Int

//struct BoardHash : Hashable, Equatable {
//    var x: UInt64
//    var y: UInt64
//
//    var hashValue: Int { return Int(x) ^ Int(y) }
//}
//
//func boardHashFromTuple(tuple: (UInt64, UInt64)) -> BoardHash {
//    return BoardHash(x: tuple.0, y: tuple.1)
//}
//
//func == (lhs: BoardHash, rhs: BoardHash) -> Bool {
//    return lhs.x == rhs.x && lhs.y == rhs.y
//}

public protocol Search {
    func search(_ boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult
}
