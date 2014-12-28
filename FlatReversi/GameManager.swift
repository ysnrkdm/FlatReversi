//
//  GameManager.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/16/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class GameManager {

    private(set) var blackPlayer: Player?
    private(set) var whitePlayer: Player?
    private(set) var hUI: HumanUserInput?
    private(set) var boardMediator: BoardMediator?
    private var gameViewModel: GameViewModel?
    private var evaluator: Evaluator?

    // just for show
    private var blackEval: Double = 0.0
    private var whiteEval: Double = 0.0
    private var debugString: String = ""

    var challengeLevelId: Int = -1

    private(set) var turn: Pieces = Pieces.None

    init() {
    }

    func initialize(gameViewModel: GameViewModel, gameSettings: GameSettings) {
        blackPlayer = getPlayerByLevel(gameSettings.blackPlayerComputer, levelId: gameSettings.blackPlayerComputerLevelId, color: Pieces.Black)
        whitePlayer = getPlayerByLevel(gameSettings.whitePlayerComputer, levelId: gameSettings.whitePlayerComputerLevelId, color: Pieces.White)

        hUI = HumanUserInput(gameManager: self)

        let b = SimpleBitBoard()
        b.initialize(8, height: 8)
        boardMediator = BoardMediator(board:b)
        boardMediator?.initializeBoard()

        self.gameViewModel = gameViewModel

        blackPlayer?.initialize(gameSettings.blackPlayerComputerLevelId)
        whitePlayer?.initialize(gameSettings.whitePlayerComputerLevelId)

        turn = Pieces.Black

        switch (challengeModeComputer()) {
        case Pieces.Black:
            challengeLevelId = gameSettings.blackPlayerComputerLevelId
        case Pieces.White:
            challengeLevelId = gameSettings.whitePlayerComputerLevelId
        default:
            NSLog("Not challenge mode")
        }


        let z = ZonesFactory().createZoneTypical7(99, bVal: 0.6, cVal: 3, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
        let ev = ClassicalEvaluator()
        ev.configure([3.0, 0.5], wEdge: [1.0, 1.0], wFixedPieces: [5.0, 3.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0], zones: z)
        self.evaluator = ev
    }

    func isChallengeMode() -> Bool {
        let blackPlayerAndHuman = blackPlayer != nil && !blackPlayer!.isComputerPlayer()
        let whitePlayerAndHuman = whitePlayer != nil && !whitePlayer!.isComputerPlayer()

        return (blackPlayerAndHuman || whitePlayerAndHuman) && !(blackPlayerAndHuman && whitePlayerAndHuman)
    }

    func challengeModeComputer() -> Pieces {
        if isChallengeMode() {
            let blackPlayerAndHuman = blackPlayer != nil && !blackPlayer!.isComputerPlayer()
            let whitePlayerAndHuman = whitePlayer != nil && !whitePlayer!.isComputerPlayer()
            if blackPlayerAndHuman {
                return Pieces.White
            } else {
                return Pieces.Black
            }
        } else {
            return Pieces.None
        }
    }

    private func getPlayerByLevel(isComputer: Bool, levelId: Int, color: Pieces) -> Player {
        var playerMediator = PlayerMediator(gameManager: self)
        if(isComputer) {
            let lc: LevelController = LevelController()

            if let player = lc.getPlayerByLevelId(levelId, playerMediator: playerMediator, color: color) {
                return player
            } else {
                let computerWeakestPlayer = RandomComputerPlayer(playerMediator: playerMediator, color: color)
                return computerWeakestPlayer
            }
        } else {
            let humanPlayer = HumanPlayer(playerMediator: playerMediator, color: color)
            return humanPlayer
        }
    }

    func configure() {

    }

    // Main game loop logic
    func startGame() {
        let br = BoardRepresentation(boardMediator: boardMediator!)
        let eval = evaluator?.evaluate(br, forPlayer: turn)
        NSLog("Game start -- now \(turn.toString()) s turn. Eval is \(eval!)")
        // Start
        switch (self.turn) {
        case Pieces.Black:
            blackEval = eval!
            blackPlayer?.play()
        case Pieces.White:
            whiteEval = eval!
            whitePlayer?.play()
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    func put(color: Pieces, x: Int, y: Int) -> Bool {
        if(self.boardMediator == nil || !self.boardMediator!.canPut(color, x: x, y: y)) {
            // No valid hand
            // If returned false, not going to next turn
            // Current player must think another hand
            NSLog("Cannot put \(x), \(y)")
            return false
        }

        let gs: GameSettings = GameSettings()
        gs.loadFromUserDefaults()

        let changes = self.boardMediator?.put(color, x: x, y: y)
        let showPuttables = isHumanTurn(nextTurn(self.turn)) && gs.showPossibleMoves
        NSLog("Put \(x), \(y)")
        if let unwrappedChanges = changes {
            if let gvm = gameViewModel {
                while(!gvm.isUpdateBoardViewQueueEmpty()) {
                    NSThread.sleepForTimeInterval(5/1000)
                }
                gvm.update(unwrappedChanges, put: [(x, y)], showPuttables: showPuttables, showAnimation: gs.showAnimation, blackEval: blackEval, whiteEval: whiteEval, debugString: debugString)
            }
        }
        self.turn = nextTurn(self.turn)
        boardMediator?.updateGuides(self.turn)
        if(isGameOver()) {
            // Game Over, show result
            NSLog("Game Over")
            // Which side is won?
            let nBlack = boardMediator?.getNumBlack()
            let nWhite = boardMediator?.getNumWhite()
            var result = Pieces.None
            if(nBlack == nWhite) {
                result = Pieces.None
            } else if (nBlack > nWhite) {
                result = Pieces.Black
            } else {
                result = Pieces.White
            }

            var message = "Black \(nBlack!) vs. White \(nWhite!)"
            var title = ""
            var showNext = false
            var nextExists = false
            var nextLabelText = ""
            // Is it challenge mode?
            let blackPlayerAndHuman = blackPlayer != nil && !blackPlayer!.isComputerPlayer()
            let whitePlayerAndHuman = whitePlayer != nil && !whitePlayer!.isComputerPlayer()
            if(isChallengeMode()) {
                let lc: LevelController = LevelController()
                if let nextLevel = lc.getNextLevel(challengeLevelId) {
                    nextLabelText = "Next level: \(nextLevel.level)"
                    nextExists = true
                }
                // If challenge mode, human won
                switch(result) {
                case Pieces.None:
                    title = "Draw."
                case Pieces.Black:
                    if(blackPlayerAndHuman) {
                        title = "You won."
                        openNextLevel()
                        showNext = true && nextExists
                    } else {
                        title = "You lose."
                    }
                case Pieces.White:
                    if(blackPlayerAndHuman) {
                        title = "You lose."
                    } else {
                        title = "You won."
                        openNextLevel()
                        showNext = true && nextExists
                    }
                default:
                    assertionFailure("Should not reach this code!")
                }
            } else {
                switch(result) {
                case Pieces.Black:
                    title = "Black won."
                case Pieces.White:
                    title = "White won."
                case Pieces.None:
                    title = "Draw."
                default:
                    assertionFailure("Should not reach this code!")
                }
            }

            self.gameViewModel?.showGameOver(title, message: message, showNext: showNext, nextLabel: nextLabelText)
        } else if (!isCurrentTurnDoablePut()) {
            NSLog("Current player cannot do anything, skipping")
            // Current player cannot do anything. Skip
            self.turn = nextTurn(self.turn)
            self.boardMediator?.updateGuides(self.turn)
            self.gameViewModel?.update([], put: [], showPuttables: isCurrentTurnHuman(), showAnimation: gs.showAnimation, blackEval: blackEval, whiteEval: whiteEval, debugString: debugString)
            self.gameViewModel?.showPasses()
            startGame()
        } else {
            // Then next player's turn
            startGame()
        }

        // If returned true, current player can stop thinking
        // hand
        return true
    }

    func isGameOver() -> Bool {
        if let unwrappedBoardMediator = boardMediator {
            // If no action from both side, game over
            let puttablesFromCurrentTurn = unwrappedBoardMediator.getPuttables(turn)
            let puttablesFromNextTurn = unwrappedBoardMediator.getPuttables(nextTurn(turn))
            if(puttablesFromCurrentTurn.count == 0 && puttablesFromNextTurn.count == 0) {
                return true
            }
        }
        return false
    }

    private func openNextLevel() {
        let lc: LevelController = LevelController()
        let nextChallengeLevel = lc.getNextLevel(challengeLevelId)
        if let uNCL = nextChallengeLevel {
            let gs: GameSettings = GameSettings()
            gs.loadFromUserDefaults()
            gs.difficultyHighestBeaten = uNCL.levelId
            gs.saveToUserDefaults()
        }
    }

    private func isCurrentTurnDoablePut() -> Bool {
        if let unwrappedBoardMediator = boardMediator {
            // If can puttable position is more than 0, doable action
            let puttablesFromCurrentTurn = unwrappedBoardMediator.getPuttables(turn)
            if(puttablesFromCurrentTurn.count > 0) {
                return true
            }
        }
        return false
    }

    func getBoardRepresentation() -> BoardRepresentation? {
        if let unwrappedBoardMediator = self.boardMediator {
            let boardRepresentation = BoardRepresentation(boardMediator: unwrappedBoardMediator)
            return boardRepresentation
        }
        return nil
    }

    func getHumanUserInput() -> HumanUserInput? {
        return hUI
    }

    func doHumanPut(x: Int, y: Int) -> Bool {
        if(!isCurrentTurnHuman()) {
            NSLog("Not human turn currently. Ignoring")
            return false
        }

        switch (turn) {
        case .Black:
            blackPlayer?.put(x, y: y)
        case .White:
            whitePlayer?.put(x, y: y)
        default:
            // Game not yet started
            return false
        }
        return true
    }

    func toString() -> String {
        if let unwrappedBoardMediator = boardMediator {
            return unwrappedBoardMediator.toString()
        }

        return "[Empty Board]"
    }

    func isCurrentTurnHuman() -> Bool {
        return isHumanTurn(self.turn)
    }

    func isHumanTurn(turn: Pieces) -> Bool {
        let blackPlayerAndHuman = turn == Pieces.Black && blackPlayer != nil && !blackPlayer!.isComputerPlayer()
        let whitePlayerAndHuman = turn == Pieces.White && whitePlayer != nil && !whitePlayer!.isComputerPlayer()
        return (blackPlayerAndHuman) || (whitePlayerAndHuman)
    }

    func setDebugString(string: String) {
        self.debugString = string
    }

    private func nextTurn(color: Pieces) -> Pieces {
        var s : Pieces = .Black
        switch color {
        case .Black:
            s = .White
        case .White:
            s = .Black
        case .None:
            s = .Black
        default:
            s = .Black
        }
        return s
    }
}