//
//  Player.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/17/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum PlayerStates {
    case Created, Initialized, Ready, Playing, Error
}

class Player {
    var playerMediator: PlayerMediator
    private(set) var color: Pieces
    private(set) var state: PlayerStates

    // MARK: Initializer

    init(playerMediator: PlayerMediator, color: Pieces) {
        self.playerMediator = playerMediator
        self.color = color
        self.state = PlayerStates.Created
    }

    // MARK: Methods cannot be overridden in sub classes

    final func isReady() -> Bool {
        return self.state == PlayerStates.Ready
    }

    final func isError() -> Bool {
        return self.state == PlayerStates.Error
    }

    final func put(x: Int, y: Int) -> Bool {
        return self.playerMediator.put(color, x: x, y: y)
    }

    final func canChangeState(from: PlayerStates, to: PlayerStates) -> Bool {
        // Catch all

        // From any states to error is allowed
        if(to == PlayerStates.Error && from != PlayerStates.Error) {
            return true
        }
        // From any states to initialized is allowed
        if(to == PlayerStates.Initialized && from != PlayerStates.Initialized) {
            return true
        }

        switch(from) {
        case PlayerStates.Created:
            return to == PlayerStates.Initialized
        case PlayerStates.Initialized:
            return to == PlayerStates.Ready
        case PlayerStates.Ready:
            return to == PlayerStates.Playing
        case PlayerStates.Playing:
            return to == PlayerStates.Ready
        case PlayerStates.Error:
            return to == PlayerStates.Ready
        }
    }

    final func changeStateTo(to: PlayerStates) -> Bool {
        if(canChangeState(self.state, to: to)) {
            self.state = to
            return true
        }

        return false
    }

    // MARK: Override below methods in sub classes

    func initialize(level: Int) {

    }

    func play() {
        
    }

    func isComputerPlayer() -> Bool {
        return true
    }

    // MARK: Multi-thread utility functions
    func dispatch_async_main(block: () -> ()) {
        dispatch_async(dispatch_get_main_queue(), block)
    }

    func dispatch_async_global(block: () -> ()) {
        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), block)
    }
}