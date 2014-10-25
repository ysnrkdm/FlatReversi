//
//  GameSettings.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/15/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class GameSettings {
    var difficultyHighestBeaten: Int = 1
    var achievements: Int = 0

    var blackPlayerComputer: Bool = false
    var blackPlayerComputerLevel: Int = 1

    var whitePlayerComputer: Bool = true
    var whitePlayerComputerLevel: Int = 1

    var showPossibleMoves: Bool = true
    var showAnimation: Bool = true

    func saveToUserDefaults() {
        let ud = NSUserDefaults.standardUserDefaults()
        ud.setInteger(difficultyHighestBeaten, forKey: "difficultyHighestBeaten")

        ud.setBool(blackPlayerComputer, forKey: "blackPlayerComputer")
        ud.setInteger(blackPlayerComputerLevel, forKey: "blackPlayerComputerLevel")

        ud.setBool(whitePlayerComputer, forKey: "whitePlayerComputer")
        ud.setInteger(whitePlayerComputerLevel, forKey: "whitePlayerComputerLevel")

        ud.setBool(showPossibleMoves, forKey: "showPossibleMoves")
        ud.setBool(showAnimation, forKey: "showAnimation")
    }

    func loadFromUserDefaults() {
        let ud = NSUserDefaults.standardUserDefaults()
        difficultyHighestBeaten = ud.integerForKey("difficultyHighestBeaten")

        blackPlayerComputer = ud.boolForKey("blackPlayerComputer")
        blackPlayerComputerLevel = ud.integerForKey("blackPlayerComputerLevel")

        whitePlayerComputer = ud.boolForKey("whitePlayerComputer")
        whitePlayerComputerLevel = ud.integerForKey("whitePlayerComputerLevel")

        showPossibleMoves = ud.boolForKey("showPossibleMoves")
        showAnimation = ud.boolForKey("showAnimation")
    }

    func validate() -> Bool {
        loadFromUserDefaults()

        if difficultyHighestBeaten <= 0 {
            return false
        }

        if blackPlayerComputerLevel <= 0 {
            return false
        }

        if whitePlayerComputerLevel <= 0 {
            return false
        }

        if blackPlayerComputerLevel > difficultyHighestBeaten || whitePlayerComputerLevel > difficultyHighestBeaten {
            return false
        }

        return true
    }

    func resetAndSave() {
        difficultyHighestBeaten = 1
        achievements = 0

        blackPlayerComputer = false
        blackPlayerComputerLevel = 1

        whitePlayerComputer = true
        whitePlayerComputerLevel = 1

        showPossibleMoves = true
        showAnimation = true

        saveToUserDefaults()
    }
}