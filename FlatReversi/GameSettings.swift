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
    var achievements: Int = 1000

    var debugDfficulty: Int = 0
    var debugAchievements: Int = 1000

    var blackPlayerComputer: Bool = false
    var blackPlayerComputerLevelId: Int = 1

    var whitePlayerComputer: Bool = true
    var whitePlayerComputerLevelId: Int = 1

    var showPossibleMoves: Bool = true
    var showAnimation: Bool = true

    var appearance: Appearance = .WhiteGray

    func saveToUserDefaults() {
        let ud = NSUserDefaults.standardUserDefaults()
        ud.setInteger(difficultyHighestBeaten, forKey: "difficultyHighestBeaten")

        ud.setBool(blackPlayerComputer, forKey: "blackPlayerComputer")
        ud.setInteger(blackPlayerComputerLevelId, forKey: "blackPlayerComputerLevelId")

        ud.setBool(whitePlayerComputer, forKey: "whitePlayerComputer")
        ud.setInteger(whitePlayerComputerLevelId, forKey: "whitePlayerComputerLevelId")

        ud.setBool(showPossibleMoves, forKey: "showPossibleMoves")
        ud.setBool(showAnimation, forKey: "showAnimation")
        ud.setObject(appearance.rawValue, forKey: "appearance")
    }

    func loadFromUserDefaults() {
        let ud = NSUserDefaults.standardUserDefaults()
        difficultyHighestBeaten = ud.integerForKey("difficultyHighestBeaten")

        blackPlayerComputer = ud.boolForKey("blackPlayerComputer")
        blackPlayerComputerLevelId = ud.integerForKey("blackPlayerComputerLevelId")

        whitePlayerComputer = ud.boolForKey("whitePlayerComputer")
        whitePlayerComputerLevelId = ud.integerForKey("whitePlayerComputerLevelId")

        showPossibleMoves = ud.boolForKey("showPossibleMoves")
        showAnimation = ud.boolForKey("showAnimation")

        if let appeIdFromUd = ud.stringForKey("appearance") {
            if let appeFromUd = Appearance(rawValue: appeIdFromUd) {
                appearance = appeFromUd
            } else {
                appearance = .WhiteGray
//                NSLog("Renewed appearance as WhiteGray because couldn't find an object for given appearance id.")
            }
        } else {
            appearance = .WhiteGray
//            NSLog("Renewed appearance as WhiteGray because couldn't find an appearance id from user-defaults.")
        }
    }

    func validate() -> Bool {
        loadFromUserDefaults()

        if difficultyHighestBeaten <= 0 {
            NSLog("difficultyHighestBeaten must be interger more than 0")
            return false
        }

        if blackPlayerComputerLevelId <= 0 {
            NSLog("blackPlayerComputerLevelId must be interger more than 0")
            return false
        }

        if whitePlayerComputerLevelId <= 0 {
            NSLog("whitePlayerComputerLevelId must be interger more than 0")
            return false
        }

        if blackPlayerComputerLevelId > achievements  {
            NSLog("blackPlayerComputerLevelId must be eq or less than achievements")
            return false
        }

        if whitePlayerComputerLevelId > achievements {
            NSLog("whitePlayerComputerLevelId must be eq or less than achievements")
            return false
        }

        return true
    }

    func resetAndSave() {
        NSLog("Resseting configuration")
        difficultyHighestBeaten = 1
        achievements = 0

        blackPlayerComputer = false
        blackPlayerComputerLevelId = 1

        whitePlayerComputer = true
        whitePlayerComputerLevelId = 1

        showPossibleMoves = true
        showAnimation = true

        saveToUserDefaults()
    }
}