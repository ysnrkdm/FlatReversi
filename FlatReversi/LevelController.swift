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
        Level(level: 5, levelId: 5, levelTitle: "Greedy", levelDescr: ""),
        Level(level: 6, levelId: 6, levelTitle: "Random + zone ordering+3", levelDescr: ""),
        Level(level: 7, levelId: 7, levelTitle: "Random + zone ordering+4", levelDescr: ""),
        Level(level: 8, levelId: 8, levelTitle: "Random + zone ordering+5", levelDescr: ""),
        Level(level: 9, levelId: 9, levelTitle: "Random + 6 PNS", levelDescr: ""),
        Level(level: 10, levelId: 10, levelTitle: "1 Depth Static Eval Search + 10 PNS", levelDescr: ""),
        Level(level: 11, levelId: 11, levelTitle: "3 Depth Static Eval Search + 10 PNS", levelDescr: ""),
        Level(level: 12, levelId: 12, levelTitle: "5 Depth Static Eval Search + 10 PNS", levelDescr: ""),
        Level(level: 13, levelId: 13, levelTitle: "Lefty PNS", levelDescr: ""),
        Level(level: 14, levelId: 14, levelTitle: "Swing left and right PNS", levelDescr: ""),
        Level(level: 15, levelId: 15, levelTitle: "Openness only search", levelDescr: ""),
        Level(level: 16, levelId: 16, levelTitle: "DepthSearcher with Perturbation", levelDescr: ""),
        Level(level: 17, levelId: 17, levelTitle: "5 Depth Static Eval Search + 10 PNS", levelDescr: ""),
        Level(level: 100, levelId: 1000, levelTitle: "To be added in next version...", levelDescr: ""),
//        Level(level: 0, levelId: 1001, levelTitle: "SpecialAI", levelDescr: ""),
    ]

    func getLevels(considerDifficultyHighestBeaten: Bool) -> [Level] {
        let gs: GameSettings = GameSettings()
        gs.loadFromUserDefaults()

        if considerDifficultyHighestBeaten && false {
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

    func getPlayerByLevelId(levelId: Int, playerMediator: PlayerMediator, color: Pieces) -> Player? {
        switch(levelId) {
        case 1:
            let rcp = RandomComputerPlayer(playerMediator: playerMediator, color: color)
            return rcp
        case 2:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical4(2, bVal: 1.1, cVal: 1.4, dVal: 1.7)
            rcp.configure(z)
            return rcp
        case 3:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical4(9, bVal: 1, cVal: 1.5, dVal: 2)
            rcp.configure(z)
            return rcp
        case 4:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical4(99, bVal: 1, cVal: 8, dVal: 16)
            rcp.configure(z)
            return rcp
        case 5:
            return GreedyPlayer(playerMediator: playerMediator, color: color)
        case 6:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical7(99, bVal: 1, cVal: 3, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
            rcp.configure(z)
            return rcp
        case 7:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical7(99, bVal: 1, cVal: 1, dVal: 3.5, eVal: 3.85, fVal: 4.1, gVal: 4.8)
            rcp.configure(z)
            return rcp
        case 8:
            let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical7(99, bVal: 0.6, cVal: -3, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
            rcp.configure(z)
            return rcp
        case 9:
            let rcp = RandomPlayerWithPNS(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical7(99, bVal: 0.6, cVal: 1, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
            rcp.configure(z, pnsLessThan: 6)
            return rcp
        case 10:
            let sssep = SimpleSearchStaticEvaluationPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical8(99, bVal: 1.6, cVal: 1, dVal: 7.5, eVal: 6.1, fVal: 4.3, gVal: 4.8, hVal: 5)
            sssep.configure(z, pnsLessThan: 10, searchDepth: 1, wPossibleMoves: [20.0, 15, 3.2, 1.1], wEdge: [1.0, 1.0], wFixedPieces: [2.0, 20.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0])
            return sssep
        case 11:
            let sssep = SimpleSearchStaticEvaluationPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical8(99, bVal: 1.6, cVal: 1, dVal: 7.5, eVal: 6.1, fVal: 4.3, gVal: 4.8, hVal: 5)
            sssep.configure(z, pnsLessThan: 10, searchDepth: 3, wPossibleMoves: [20.0, 15, 3.2, 1.1], wEdge: [1.0, 1.0], wFixedPieces: [2.0, 20.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0])
            return sssep
        case 12:
            let sssep = TranspositionedAlphaBetaSearchWithEvaluationPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical8(99, bVal: 1.6, cVal: -5, dVal: 7.5, eVal: 6.1, fVal: 4.3, gVal: 4.8, hVal: 5)
            sssep.configure(z, pnsLessThan: 10, searchDepth: 5, wPossibleMoves: [20.0, 15, 3.2, 1.1], wEdge: [1.0, 1.0], wFixedPieces: [2.0, 400.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0])
            return sssep
        case 13:
            let rcp = LeftyPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical7(99, bVal: 0.6, cVal: 1, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
            rcp.configure(z, pnsLessThan: 10)
            return rcp
        case 14:
            let rcp = SwingPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical7(99, bVal: 0.6, cVal: 1, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
            rcp.configure(z, pnsLessThan: 10)
            return rcp
        case 15:
            let sssep = TranspositionedAlphaBetaSearchWithEvaluationPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneUniform(1.0)
            sssep.configure(z, pnsLessThan: 10, searchDepth: 3, wPossibleMoves: [0], wEdge: [0], wFixedPieces: [0], wOpenness: [1], wBoardEvaluation: [0])
            return sssep
        case 16:
            let sssep = TreeSearchWithPerturbationPlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical8(99, bVal: 1.6, cVal: -5, dVal: 7.5, eVal: 6.1, fVal: 4.3, gVal: 4.8, hVal: 5)
            sssep.configure(z, pnsLessThan: 10, searchDepth: 3, wPossibleMoves: [20.0, 15, 3.2, 1.1], wEdge: [1.0, 1.0], wFixedPieces: [2.0, 20.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0], randomThreshold: 0.1)
            return sssep
        case 17:
            let sssep = TranspositionAlphaBetaSimplePlayer(playerMediator: playerMediator, color: color)
            let z = ZonesFactory().createZoneTypical8(999, bVal: 1.6, cVal: -5, dVal: 7.5, eVal: 6.1, fVal: 4.3, gVal: 4.8, hVal: 5)
            sssep.configure(z, pnsLessThan: 10, searchDepth: 5, wPossibleMoves: [20.0, 15, 3.2, 1.1], wEdge: [1.0, 1.0], wFixedPieces: [2.0, 400.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0])
            return sssep

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