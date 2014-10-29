//
//  ClassicalEvaluator.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/28/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class ClassicalEvaluator: Evaluator {
    override func eval(board: BoardRepresentation) -> Double {
        return 0.0
    }

    // MARK: Private functions

    private func phase(board: BoardRepresentation) -> Int {
        return 1
    }
}