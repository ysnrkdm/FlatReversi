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
    var implClass: String

    init(level: Int, levelId: Int, levelTitle: String, levelDescr: String, implClass: String) {
        self.level = level
        self.levelId = levelId
        self.levelTitle = levelTitle
        self.levelDescr = levelDescr
        self.implClass = implClass
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
        Level(level: 0, levelId: 0, levelTitle: "Human", levelDescr: "Human Player", implClass: "HumanPlayer"),
        Level(level: 1, levelId: 1, levelTitle: "Random", levelDescr: "", implClass: "RandomComputerPlayer"),
        Level(level: 2, levelId: 2, levelTitle: "Random + zone ordering", levelDescr: "", implClass: "RandomComputerPlayer"),
        Level(level: 3, levelId: 3, levelTitle: "Random + zone ordering+", levelDescr: "", implClass: "RandomComputerPlayer"),
        Level(level: 4, levelId: 4, levelTitle: "Random + zone ordering+2", levelDescr: "", implClass: "RandomComputerPlayer"),
        Level(level: 0, levelId: 1004, levelTitle: "SpecialAI", levelDescr: "", implClass: "RandomComputerPlayer"),
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

    func getPlayerByLevelId(levelId: Int, playerMediattor: PlayerMediator, color: Pieces) -> Player? {
        if let level = getLevelByLevelId(levelId) {
            var anyobjectype : AnyObject.Type = NSClassFromString("FlatReversi." + level.implClass)
            var nsobjectype : NSObject.Type = anyobjectype as NSObject.Type
            var rec: AnyObject = nsobjectype()
            if let player = rec as? Player {
                return player
            }
        }
        return nil
    }

    func getName(classType:AnyClass) -> String {

        let classString = NSStringFromClass(classType.self)
        let range = classString.rangeOfString(".", options: NSStringCompareOptions.CaseInsensitiveSearch, range: Range<String.Index>(start:classString.startIndex, end: classString.endIndex), locale: nil)
        return classString.substringFromIndex(range!.endIndex)
    }
}