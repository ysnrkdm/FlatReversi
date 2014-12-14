//
//  Search.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/12/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class SearchResult {
    var value: Double
    var pv: [(Int, Int)]

    //
    var nodesSearched = 0
    var elapsedTimeInSec = 0.0
    var transpositionHitCount = 0
    var nps: Double {
        return Double(self.nodesSearched) / elapsedTimeInSec
    }

    init(value: Double, pv: [(Int, Int)]) {
        self.value = value
        self.pv = pv
    }

    func toString() -> String {
        let transRatio: Double = Double(transpositionHitCount) / Double(nodesSearched) * 100.0
        return "\(value) - pv \(pv) : searched \(nodesSearched) (\(transRatio)%% transposition table hit) nodes in \(elapsedTimeInSec). NPS is \(nps)"
    }

    func toShortString() -> String {
        let eltime = String(format: "%.01f", Float(elapsedTimeInSec))
        let npsStr = String(format: "%.01f", Float(nps))

        return "Searched in \(eltime) secs. NPS is \(npsStr)"

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

func == (lhs: BoardHash, rhs: BoardHash) -> Bool {
    return lhs.x == rhs.x && lhs.y == rhs.y
}

protocol Search {
    func search(boardRepresentation: BoardRepresentation, forPlayer: Pieces, evaluator: Evaluator, depth: Int) -> SearchResult
}