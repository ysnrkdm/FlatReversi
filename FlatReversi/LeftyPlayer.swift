//
//  LeftyPlayer.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/14/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class LeftyPlayer: ComputerPlayer {
    func configure(_ zones: Zones, pnsLessThan: Int) {
        self.thinker = LeftyThink(zones: zones, pnsLessThan: pnsLessThan)
    }
}
