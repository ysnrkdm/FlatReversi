//
//  PlayerMediator.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/18/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class PlayerMediator {
    var gameManager: GameManager

    init(gameManager:GameManager) {
        self.gameManager = gameManager
    }

    func put(_ color: Pieces, x: Int, y: Int) -> Bool {
        return gameManager.put(color, x: x, y: y)
    }

    func getBoardRepresentation() -> BoardRepresentation? {
        return gameManager.getBoardRepresentation()
    }
}
