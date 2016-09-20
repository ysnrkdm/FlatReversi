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
        let ud = UserDefaults.standard
        ud.set(difficultyHighestBeaten, forKey: "difficultyHighestBeaten")

        ud.set(blackPlayerComputer, forKey: "blackPlayerComputer")
        ud.set(blackPlayerComputerLevelId, forKey: "blackPlayerComputerLevelId")

        ud.set(whitePlayerComputer, forKey: "whitePlayerComputer")
        ud.set(whitePlayerComputerLevelId, forKey: "whitePlayerComputerLevelId")

        ud.set(showPossibleMoves, forKey: "showPossibleMoves")
        ud.set(showAnimation, forKey: "showAnimation")
        ud.set(appearance.rawValue, forKey: "appearance")
    }

    func loadFromUserDefaults() {
        let ud = UserDefaults.standard
        difficultyHighestBeaten = ud.integer(forKey: "difficultyHighestBeaten")

        blackPlayerComputer = ud.bool(forKey: "blackPlayerComputer")
        blackPlayerComputerLevelId = ud.integer(forKey: "blackPlayerComputerLevelId")

        whitePlayerComputer = ud.bool(forKey: "whitePlayerComputer")
        whitePlayerComputerLevelId = ud.integer(forKey: "whitePlayerComputerLevelId")

        showPossibleMoves = ud.bool(forKey: "showPossibleMoves")
        showAnimation = ud.bool(forKey: "showAnimation")

        if let appeIdFromUd = ud.string(forKey: "appearance") {
            if let appeFromUd = Appearance(rawValue: appeIdFromUd) {
                appearance = appeFromUd
            } else {
                appearance = .WhiteGray
            }
        } else {
            appearance = .WhiteGray
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
