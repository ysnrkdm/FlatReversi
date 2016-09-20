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
    func initialize(_ width: Int, height: Int) { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Basic functions
    func withinBoard(_ x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }

    func set(_ color: Pieces, x: Int, y: Int) { fatalError("Implement actual class by inheriting this class.") }
    func get(_ x: Int, y: Int) -> Pieces { fatalError("Implement actual class by inheriting this class.") }
    func isPieceAt(_ piece: Pieces, x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    func put(_ color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }

    func width() -> Int { fatalError("Implement actual class by inheriting this class.") }
    func height() -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Query functions
    func getNumBlack() -> Int { fatalError("Implement actual class by inheriting this class.") }
    func getNumWhite() -> Int { fatalError("Implement actual class by inheriting this class.") }

    func canPut(_ color: Pieces, x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    func getPuttables(_ color: Pieces) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }
    func isAnyPuttable(_ color: Pieces) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    func getReversible(_ color: Pieces, x: Int, y: Int) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }
    func isEmpty(_ x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }

    func numPeripherals(_ color: Pieces, x: Int, y: Int) -> Int { fatalError("Implement actual class by inheriting this class.") }

    func hashValue() -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Update functions
    func updateGuides(_ color: Pieces) -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Utility functions
    func clone() -> Board { fatalError("Implement actual class by inheriting this class.") }
    func cloneBitBoard() -> FastBitBoard { fatalError("Implement actual class by inheriting this class.") }

    func toString() -> String { fatalError("Implement actual class by inheriting this class.") }

    func isTerminal() -> Bool { fatalError("Implement actual class by inheriting this class.") }

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
}
