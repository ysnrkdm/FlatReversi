//
//  Utils.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/14/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

func sum (_ array : [Int]) -> Int {
    if array.isEmpty {
        return 0
    }
    return array.reduce(array[0], {$0 + $1})
}

func min (_ array : [Int]) -> Int {
    if array.isEmpty {
        return Int.max
    }
    return array.reduce(array[0], {$0 > $1 ? $1 : $0})
}

func min(_ a: Double, b: Double) -> Double {
    return a > b ? b : a
}

func max (_ array : [Int]) -> Int {
    if array.isEmpty {
        return Int.min
    }
    return array.reduce(array[0], {$0 < $1 ? $1 : $0})
}

func max (_ array : [Double]) -> Double {
    if array.isEmpty {
        return 0.0
    }
    return array.reduce(array[0], {$0 < $1 ? $1 : $0})
}

func max(_ a: Double, b: Double) -> Double {
    return a > b ? a : b
}

func nextTurn(_ color: Pieces) -> Pieces {
    var s : Pieces = .black
    switch color {
    case .black:
        s = .white
    case .white:
        s = .black
    default:
        s = .black
    }
    return s
}

func LOG(    body: String!,
             function: String = #function,
             line: Int = #line)
{
    let datestr = String(NSDate().description);
    print("[\(datestr) : \(function) @ \(line)] \(body)")
}
