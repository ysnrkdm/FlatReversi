//
//  Utils.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/14/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

func sum (var array : [Int]) -> Int {
    if array.isEmpty {
        return 0
    }
    return reduce(array, array[0]) {$0 + $1}
}

func min (var array : [Int]) -> Int {
    if array.isEmpty {
        return Int.max
    }
    return reduce(array, array[0]) {$0 > $1 ? $1 : $0}
}

func max (var array : [Int]) -> Int {
    if array.isEmpty {
        return Int.min
    }
    return reduce(array, array[0]) {$0 < $1 ? $1 : $0}
}

func max (var array : [Double]) -> Double {
    if array.isEmpty {
        return 0.0
    }
    return reduce(array, array[0]) {$0 < $1 ? $1 : $0}
}