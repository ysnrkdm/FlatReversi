//
//  ComputerPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/27/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class ComputerPlayer: Player {

    var level: Int = 0

    override func initialize(_ level: Int) {
        self.level = level
    }

    func configure() {
        NSLog("Define configure method with arguments.")
    }

    override func play() {
        dispatch_async_global({self.think()})
    }

    func think() {
        assertionFailure("Override this method and implement at child class.")
    }
}
