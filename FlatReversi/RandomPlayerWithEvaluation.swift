//
//  RandomPlayerWithEvaluation.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
import Graphene

class RandomPlayerWithEvaluation: ComputerPlayer {
    func configure(_ zones: Zones) {
        self.thinker = RandomWithWeightsThink(zones: zones)
    }
}
