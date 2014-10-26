//
//  LevelController.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/25/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class Level {
    var level: Int
    var levelId: Int
    var levelTitle: String
    var levelDescr: String

    init(level: Int, levelId: Int, levelTitle: String, levelDescr: String) {
        self.level = level
        self.levelId = levelId
        self.levelTitle = levelTitle
        self.levelDescr = levelDescr
    }

    func toString() -> String {
        if level > 0 {
            return "Level \(level) - \(levelTitle)"
        } else {
            return "AI - \(levelTitle)"
        }
    }
}

class LevelController {
    //
    var levels: [Level] = [
        Level(level: 0, levelId: 0, levelTitle: "Human", levelDescr: "Human Player"),
        Level(level: 1, levelId: 1, levelTitle: "Random", levelDescr: ""),
        Level(level: 2, levelId: 2, levelTitle: "Random + zone ordering", levelDescr: ""),
        Level(level: 3, levelId: 3, levelTitle: "Random + zone ordering+", levelDescr: ""),
        Level(level: 4, levelId: 4, levelTitle: "Random + zone ordering+2", levelDescr: ""),
        Level(level: 0, levelId: 1001, levelTitle: "SpecialAI", levelDescr: ""),
    ]

    func getLevels(considerDifficultyHighestBeaten: Bool) -> [Level] {
        let gs: GameSettings = GameSettings()
        gs.loadFromUserDefaults()

        if(considerDifficultyHighestBeaten) {
            var ret: [Level] = []
            for level in levels {
                if 0 < level.level && level.level <= gs.difficultyHighestBeaten {
                    ret.append(level)
                }
                if 0 == level.level && level.levelId <= gs.achievements {
                    ret.append(level)
                }
            }
            return ret
        } else {
            return levels
        }
    }

    func getLevelByLevelId(levelId: Int) -> Level? {
        for level in levels {
            if level.levelId == levelId {
                return level
            }
        }
        return nil
    }

    func getLevelIdByLevel(level: Int) -> [Int] {
        var ret: [Int] = []
        for lv in levels {
            if level == lv.level {
                ret += [lv.levelId]
            }
        }
        return ret
    }

    func isAchievementAI(level: Level) -> Bool {
        return isAchievementAILevelId(level.levelId)
    }

    func isAchievementAILevelId(levelId: Int) -> Bool {
        return levelId > 1000
    }

    func isNullAI(level: Level) -> Bool {
        return isNullAILevelId(level.levelId)
    }

    func isNullAILevelId(levelId: Int) -> Bool {
        return levelId == 0 || levelId == 1000
    }

    func getPlayerByLevelId(levelId: Int, playerMediattor: PlayerMediator, color: Pieces) -> Player? {
        switch(levelId) {
        case 1:
            let rcp = RandomComputerPlayer(playerMediator: playerMediattor, color: color)
            return rcp
        case 2:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediattor, color: color)
            let z = ZonesFactory().createZoneTypical4(2, bVal: 1.1, cVal: 1.4, dVal: 1.7)
            rcp.configure(z)
            return rcp
        case 3:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediattor, color: color)
            let z = ZonesFactory().createZoneTypical4(9, bVal: 1, cVal: 1.5, dVal: 2)
            rcp.configure(z)
            return rcp
        case 4:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediattor, color: color)
            let z = ZonesFactory().createZoneTypical4(99, bVal: 1, cVal: 8, dVal: 16)
            rcp.configure(z)
            return rcp
        default:
            return nil
        }
    }

    func getRandomComputerPlayerConfigured(playerMediattor: PlayerMediator, color: Pieces, maxCandidates: Int, zones: [[Int]]) -> RandomComputerPlayer? {
        return nil
    }

    func getName(classType:AnyClass) -> String {

        let classString = NSStringFromClass(classType.self)
        let range = classString.rangeOfString(".", options: NSStringCompareOptions.CaseInsensitiveSearch, range: Range<String.Index>(start:classString.startIndex, end: classString.endIndex), locale: nil)
        return classString.substringFromIndex(range!.endIndex)
    }

    func getNextLevelId(levelId: Int) -> Int {
        if isNullAILevelId(levelId) || isAchievementAILevelId(levelId) {
            return -1
        } else {
            if let currentLevel = getLevelByLevelId(levelId) {
                let nl = getLevelIdByLevel(currentLevel.level + 1)
                if nl.count > 0 {
                    return nl[0]
                }
            }
        }
        return -1
    }

    func getNextLevel(levelId: Int) -> Level? {
        let nextLevelId = getNextLevelId(levelId)
        if nextLevelId > 0 {
            return getLevelByLevelId(nextLevelId)
        } else {
            return nil
        }
    }
}