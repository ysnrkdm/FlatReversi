//
//  Board.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum Pieces {
    case Black, White, Empty, Guide, None

    func toString() -> String {
        switch self {
        case .Black:
            return "B"
        case .White:
            return "W"
        case .Empty:
            return "."
        case .Guide:
            return "@"
        case .None:
            return "*"
        default:
            return "!"
        }
    }
}

protocol Board {
    func initialize(width: Int, height: Int)

    // MARK: Basic functions
    func withinBoard(x: Int, y: Int) -> Bool

    func set(color: Pieces, x: Int, y: Int)
    func get(x: Int, y: Int) -> Pieces
    func isPieceAt(piece: Pieces, x: Int, y: Int) -> Bool
    func put(color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)]

    func width() -> Int
    func height() -> Int

    // MARK: Query functions
    func getNumBlack() -> Int
    func getNumWhite() -> Int

    func canPut(color: Pieces, x: Int, y: Int) -> Bool
    func getPuttables(color: Pieces) -> [(Int, Int)]
    func isAnyPuttable(color: Pieces) -> Bool
    func getReversible(color: Pieces, x: Int, y: Int) -> [(Int, Int)]
    func isEmpty(x: Int, y: Int) -> Bool

    func numPeripherals(color: Pieces, x: Int, y: Int) -> Int

    func hashValue() -> Int

    // MARK: Update functions
    func updateGuides(color: Pieces) -> Int

    // MARK: Utility functions
    func clone() -> Board

    func toString() -> String
}
