//
//  GameSettings.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/15/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class GameSettings {
    var difficultyLastChallenged: Int = 0
    var blackPlayerComputer: Int = 0
    var whitePlayerComputer: Int = 0
    var showPossibleMoves: Bool = true
    var showAnimation: Bool = true

    func saveToUserDefaults() {
        let ud = NSUserDefaults.standardUserDefaults()
        ud.setInteger(self.difficultyLastChallenged, forKey: "difficultyLastChallenged")
        ud.setInteger(blackPlayerComputer, forKey: "blackPlayerComputer")
        ud.setInteger(whitePlayerComputer, forKey: "whitePlayerComputer")
        ud.setBool(showPossibleMoves, forKey: "showPossibleMoves")
        ud.setBool(showAnimation, forKey: "showAnimation")
    }

    func loadFromUserDefaults() {
        let ud = NSUserDefaults.standardUserDefaults()
        difficultyLastChallenged = ud.integerForKey("difficultyLastChallenged")
        blackPlayerComputer = ud.integerForKey("blackPlayerComputer")
        whitePlayerComputer = ud.integerForKey("whitePlayerComputer")
        showPossibleMoves = ud.boolForKey("showPossibleMoves")
        showAnimation = ud.boolForKey("showAnimation")
    }
}