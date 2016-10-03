//
//  Board.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/13/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

public enum Pieces {
    case black, white, empty, guide, none

    public func toString() -> String {
        switch self {
        case .black:
            return "B"
        case .white:
            return "W"
        case .empty:
            return "."
        case .guide:
            return "@"
        case .none:
            return "*"
        }
    }
}

public protocol Board {
    func initialize(_ width: Int, height: Int)

    // MARK: Basic functions
    func withinBoard(_ x: Int, y: Int) -> Bool

    func set(_ color: Pieces, x: Int, y: Int)
    func get(_ x: Int, y: Int) -> Pieces
    func isPieceAt(_ piece: Pieces, x: Int, y: Int) -> Bool
    func put(_ color: Pieces, x: Int, y: Int, guides: Bool, returnChanges: Bool) -> [(Int, Int)]

    func width() -> Int
    func height() -> Int

    // MARK: Query functions
    func getNumBlack() -> Int
    func getNumWhite() -> Int

    func canPut(_ color: Pieces, x: Int, y: Int) -> Bool
    func getPuttables(_ color: Pieces) -> [(Int, Int)]
    func isAnyPuttable(_ color: Pieces) -> Bool
    func getReversible(_ color: Pieces, x: Int, y: Int) -> [(Int, Int)]
    func isEmpty(_ x: Int, y: Int) -> Bool

    func numPeripherals(_ color: Pieces, x: Int, y: Int) -> Int

    func hashValue() -> Int

    // MARK: Update functions
    func updateGuides(_ color: Pieces) -> Int

    // MARK: Utility functions
    func clone() -> Board

    func toString() -> String
}
