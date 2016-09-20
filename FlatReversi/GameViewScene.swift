//
//  GameViewScene.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/31/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

protocol GameViewScene {
    func updateView(_ bd : BoardMediator, changes:[(Int, Int)], put: [(Int, Int)], showPuttables: Bool, showAnimation: Bool, blackEval: Double, whiteEval: Double, debugString: String)

    func showPasses()

    func showGameOver(_ title:String, message: String, showNext: Bool, nextLabel: String)

    func isUpdateBoardViewQueueEmpty() -> Bool
}
