//
//  FasterBitBoard.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/16/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class FastBitBoard : Board {
    func getUnsafeBitBoard() -> BitBoard { assertionFailure("Implement actual class by inheriting this class.") }

    // MARK: Board functions
    func initialize(width: Int, height: Int) { assertionFailure("Implement actual class by inheriting this class.") }

    // MARK: Basic functions
    func withinBoard(x: Int, y: Int) -> Bool { assertionFailure("Implement actual class by inheriting this class.") }

    func set(color: Pieces, x: Int, y: Int) { assertionFailure("Implement actual class by inheriting this class.") }
    func get(x: Int, y: Int) -> Pieces { assertionFailure("Implement actual class by inheriting this class.") }
    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool { assertionFailure("Implement actual class by inheriting this class.") }
    func put(color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)] { assertionFailure("Implement actual class by inheriting this class.") }

    func width() -> Int { assertionFailure("Implement actual class by inheriting this class.") }
    func height() -> Int { assertionFailure("Implement actual class by inheriting this class.") }

    // MARK: Query functions
    func getNumBlack() -> Int { assertionFailure("Implement actual class by inheriting this class.") }
    func getNumWhite() -> Int { assertionFailure("Implement actual class by inheriting this class.") }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool { assertionFailure("Implement actual class by inheriting this class.") }
    func getPuttables(color: Pieces) -> [(Int, Int)] { assertionFailure("Implement actual class by inheriting this class.") }
    func isAnyPuttable(color: Pieces) -> Bool { assertionFailure("Implement actual class by inheriting this class.") }
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] { assertionFailure("Implement actual class by inheriting this class.") }
    func isEmpty(x: Int, y: Int) -> Bool { assertionFailure("Implement actual class by inheriting this class.") }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int { assertionFailure("Implement actual class by inheriting this class.") }

    func hashValue() -> Int { assertionFailure("Implement actual class by inheriting this class.") }

    // MARK: Update functions
    func updateGuides(color: Pieces) -> Int { assertionFailure("Implement actual class by inheriting this class.") }

    // MARK: Utility functions
    func clone() -> Board { assertionFailure("Implement actual class by inheriting this class.") }
    func cloneBitBoard() -> FastBitBoard { assertionFailure("Implement actual class by inheriting this class.") }

    func toString() -> String { assertionFailure("Implement actual class by inheriting this class.") }

    func isTerminal() -> Bool { assertionFailure("Implement actual class by inheriting this class.") }

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
}