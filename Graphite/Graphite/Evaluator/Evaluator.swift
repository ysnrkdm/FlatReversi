//
//  Evaluator.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/28/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

public protocol Evaluator {
    func evaluate(_ board: BoardRepresentation, forPlayer: Pieces) -> Double
}

public class BitBoardEvaluator : Evaluator {
    public func evaluate(_ board: BoardRepresentation, forPlayer: Pieces) -> Double {
        fatalError("Implement this function by inheriting class.")
    }

    public func evaluateBitBoard(_ board: BitBoard, forPlayer: Pieces) -> Double {
        fatalError("Implement this function by inheriting class.")
    }
}
