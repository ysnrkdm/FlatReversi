//
//  Evaluator.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/28/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

protocol Evaluator {
    func evaluate(board: BoardRepresentation, forPlayer: Pieces) -> Double
}

class BitBoardEvaluator : Evaluator {
    func evaluate(board: BoardRepresentation, forPlayer: Pieces) -> Double {
        fatalError("Implement this function by inheriting class.")
    }

    func evaluateBitBoard(board: BitBoard, forPlayer: Pieces) -> Double {
        fatalError("Implement this function by inheriting class.")
    }
}