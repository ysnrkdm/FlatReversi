//
//  Utils.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/14/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

func sum (array : [Int]) -> Int {
    if array.isEmpty {
        return 0
    }
    return array.reduce(array[0], combine: {$0 + $1})
}

func min (array : [Int]) -> Int {
    if array.isEmpty {
        return Int.max
    }
    return array.reduce(array[0], combine: {$0 > $1 ? $1 : $0})
}

func min(a: Double, b: Double) -> Double {
    return a > b ? b : a
}

func max (array : [Int]) -> Int {
    if array.isEmpty {
        return Int.min
    }
    return array.reduce(array[0], combine: {$0 < $1 ? $1 : $0})
}

func max (array : [Double]) -> Double {
    if array.isEmpty {
        return 0.0
    }
    return array.reduce(array[0], combine: {$0 < $1 ? $1 : $0})
}

func max(a: Double, b: Double) -> Double {
    return a > b ? a : b
}

func nextTurn(color: Pieces) -> Pieces {
    var s : Pieces = .Black
    switch color {
    case .Black:
        s = .White
    case .White:
        s = .Black
    default:
        s = .Black
    }
    return s
}