//
//  FasterBitBoard.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/16/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

public class FastBitBoard : Board {
    public func getUnsafeBitBoard() -> BitBoard { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Board functions
    public func initialize(_ width: Int, height: Int) { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Basic functions
    public func withinBoard(_ x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }

    public func set(_ color: Pieces, x: Int, y: Int) { fatalError("Implement actual class by inheriting this class.") }
    public func get(_ x: Int, y: Int) -> Pieces { fatalError("Implement actual class by inheriting this class.") }
    public func isPieceAt(_ piece: Pieces, x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    public func put(_ color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }

    public func width() -> Int { fatalError("Implement actual class by inheriting this class.") }
    public func height() -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Query functions
    public func getNumBlack() -> Int { fatalError("Implement actual class by inheriting this class.") }
    public func getNumWhite() -> Int { fatalError("Implement actual class by inheriting this class.") }

    public func canPut(_ color: Pieces, x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    public func getPuttables(_ color: Pieces) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }
    public func isAnyPuttable(_ color: Pieces) -> Bool { fatalError("Implement actual class by inheriting this class.") }
    public func getReversible(_ color: Pieces, x: Int, y: Int) -> [(Int, Int)] { fatalError("Implement actual class by inheriting this class.") }
    public func isEmpty(_ x: Int, y: Int) -> Bool { fatalError("Implement actual class by inheriting this class.") }

    public func numPeripherals(_ color: Pieces, x: Int, y: Int) -> Int { fatalError("Implement actual class by inheriting this class.") }

    public func hashValue() -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Update functions
    public func updateGuides(_ color: Pieces) -> Int { fatalError("Implement actual class by inheriting this class.") }

    // MARK: Utility functions
    public func clone() -> Board { fatalError("Implement actual class by inheriting this class.") }
    func cloneBitBoard() -> FastBitBoard { fatalError("Implement actual class by inheriting this class.") }

    public func toString() -> String { fatalError("Implement actual class by inheriting this class.") }

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
