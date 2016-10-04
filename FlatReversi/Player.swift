//
//  Player.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphite

enum PlayerStates {
    case created, initialized, ready, playing, error
}

class Player {
    var playerMediator: PlayerMediator
    fileprivate(set) var color: Pieces
    fileprivate(set) var state: PlayerStates

    // MARK: Initializer

    init(playerMediator: PlayerMediator, color: Pieces) {
        self.playerMediator = playerMediator
        self.color = color
        self.state = PlayerStates.created
    }

    // MARK: Methods cannot be overridden in sub classes

    final func isReady() -> Bool {
        return self.state == PlayerStates.ready
    }

    final func isError() -> Bool {
        return self.state == PlayerStates.error
    }

    final func put(_ x: Int, y: Int) -> Bool {
        return self.playerMediator.put(color, x: x, y: y)
    }

    final func canChangeState(_ from: PlayerStates, to: PlayerStates) -> Bool {
        // Catch all

        // From any states to error is allowed
        if(to == PlayerStates.error && from != PlayerStates.error) {
            return true
        }
        // From any states to initialized is allowed
        if(to == PlayerStates.initialized && from != PlayerStates.initialized) {
            return true
        }

        switch(from) {
        case PlayerStates.created:
            return to == PlayerStates.initialized
        case PlayerStates.initialized:
            return to == PlayerStates.ready
        case PlayerStates.ready:
            return to == PlayerStates.playing
        case PlayerStates.playing:
            return to == PlayerStates.ready
        case PlayerStates.error:
            return to == PlayerStates.ready
        }
    }

    final func changeStateTo(_ to: PlayerStates) -> Bool {
        if(canChangeState(self.state, to: to)) {
            self.state = to
            return true
        }

        return false
    }

    // MARK: Override below methods in sub classes

    func initialize(_ level: Int) {

    }

    func play() {
        
    }

    func isComputerPlayer() -> Bool {
        return true
    }
}
