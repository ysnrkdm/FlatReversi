//
//  HumanPlayer.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class HumanPlayer: Player {

    override func initialize(level: Int) {
        changeStateTo(PlayerStates.Initialized)
        changeStateTo(PlayerStates.Ready)
    }
    override func play() {
    }

    override func isComputerPlayer() -> Bool {
        return false
    }
}