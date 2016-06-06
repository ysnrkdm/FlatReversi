//
//  HumanUserInput.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/19/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class HumanUserInput {
    var gameManager: GameManager

    init(gameManager: GameManager) {
        self.gameManager = gameManager
    }

    func put(x: Int, y: Int) -> Bool {
        return gameManager.doHumanPut(x, y: y)
    }
}