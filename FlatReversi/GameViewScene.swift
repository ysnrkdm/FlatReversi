//
//  GameViewScene.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/31/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

protocol GameViewScene {
    func updateView(bd : BoardMediator, changes:[(Int, Int)], put: [(Int, Int)], showPuttables: Bool, showAnimation: Bool)

    func showPasses()

    func showGameOver(title:String, message: String, showNext: Bool, nextLabel: String)

    func isUpdateBoardViewQueueEmpty() -> Bool
}