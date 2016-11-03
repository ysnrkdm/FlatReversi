//
//  RandomPlayerWithPNS.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 11/6/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class RandomPlayerWithPNS: ComputerPlayer {
    func configure(_ zones: Zones, pnsLessThan: Int) {
        self.thinker = RandomWithPNSThink(zones: zones, pnsLessThan: pnsLessThan)
    }
}
