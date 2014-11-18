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
//    var board : [[Pieces]] = []
//    var _height = 8
//    var _width = 8
//    var height: Int {
//        return _height
//    }
//    var width: Int {
//        return _width
//    }

    func initialize(width: Int, height: Int)

    func withinBoard(x: Int, y: Int) -> Bool

    func set(color: Pieces, x: Int, y: Int)

    func get(x: Int, y: Int) -> Pieces

    func width() -> Int

    func height() -> Int

    func clone() -> Board

    func toString() -> String
}