//
//  FasterBitBoard.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/16/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class FastBitBoard : Board {
    func getUnsafeBitBoard() -> BitBoard { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Board functions
    func initialize(width: Int, height: Int) { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Basic functions
    func withinBoard(x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }

    func set(color: Pieces, x: Int, y: Int) { fatalError("Implement actual class by inheriting this class.") }
    func get(x: Int, y: Int) -> Pieces { fatalError("Implement actual class by inheriting this class.") }
    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    func put(color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }

    func width() -> Int { fatalError("Implement actual class by inheriting this class.") }
    func height() -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Query functions
    func getNumBlack() -> Int { fatalError("Implement actual class by inheriting this class.") }
    func getNumWhite() -> Int { fatalError("Implement actual class by inheriting this class.") }

    func canPut(color: Pieces, x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    func getPuttables(color: Pieces) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }
    func isAnyPuttable(color: Pieces) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }
    func isEmpty(x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int { fatalError("Implement actual class by inheriting this class.") }

    func hashValue() -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Update functions
    func updateGuides(color: Pieces) -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Utility functions
    func clone() -> Board { fatalError("Implement actual class by inheriting this class.") }
    func cloneBitBoard() -> FastBitBoard { fatalError("Implement actual class by inheriting this class.") }

    func toString() -> String { fatalError("Implement actual class by inheriting this class.") }

    func isTerminal() -> Bool { fatalError("Implement actual class by inheriting this class.") }

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