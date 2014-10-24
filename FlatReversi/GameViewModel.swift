//
//  GameViewModel.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/19/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class GameViewModel {
    var gameManager: GameManager
    var view: GameScene

    init(gameManager: GameManager, view: GameScene) {
        self.view = view
        self.gameManager = gameManager
    }

    func update(changes:[(Int, Int)], put: [(Int, Int)], showPuttables: Bool) {
        // from GameManager
        // to GameScene
        view.updateView(gameManager.boardMediator!, changes: changes, put: put, showPuttables: showPuttables)
    }

    func showPasses() {
        view.showPasses()
    }

    func showGameOver(title:String, message: String, showNext: Bool, nextLabel: String) {
        view.showGameOver(title, message: message, showNext: showNext, nextLabel: nextLabel)
    }
}