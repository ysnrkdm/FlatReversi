//
//  GreedyPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/27/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class GreedyPlayer: ComputerPlayer {
    override func configure() {
        self.thinker = GreedyThink()
    }
}
