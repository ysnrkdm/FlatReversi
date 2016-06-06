//
//  ProofSolver.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/29/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum Proofs {
    case BlackWin, WhiteWin, Draw, Undefined

    func toString() -> String {
        switch self {
        case BlackWin:
            return "Black Win"
        case WhiteWin:
            return "White Win"
        case Draw:
            return "Draw"
        case Undefined:
            return "Undefined"
        }
    }
}

class ProofAnswer {
    var proof: Proofs
    var moves: [(Int, Int)]

    init(proof: Proofs, moves: [(Int, Int)]) {
        self.proof = proof
        self.moves = moves
    }

    func toString() -> String {
        let pf = proof.toString()
        return pf + "\(moves)"
    }
}

class ProofSolver {
    func solve(boardRepresentation: BoardRepresentation, forPlayer: Pieces) -> ProofAnswer {
        return ProofAnswer(proof: .Undefined, moves: [])
    }

}