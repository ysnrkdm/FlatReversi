//
//  GameManager.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/16/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation
fileprivate func < <T : Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l < r
  case (nil, _?):
    return true
  default:
    return false
  }
}

fileprivate func > <T : Comparable>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return l > r
  default:
    return rhs < lhs
  }
}


class GameManager {
    /// Basic components, meaning, 2 players and board with pieces.
    fileprivate(set) var blackPlayer: Player?
    fileprivate(set) var whitePlayer: Player?
    fileprivate(set) var boardMediator: BoardMediator?
    /// Managing who's turn is GameManager's job.
    fileprivate(set) var turn: Pieces = Pieces.none

    /// Input control from user
    fileprivate(set) var hUI: HumanUserInput?

    /// GameManager <-> GameViewModel -> GameViewScene
    /// which is in MVVM context (Mode, View, View-Model).
    /// From the deifition of MVVM, only M <-> VM -> V is allowed.
    /// (allow means the stream of the data and instruction.
    /// Source of allow can call the destionaion, to pass
    /// information and instruction.
    /// Here:
    /// GameManager represents M,
    /// GameViewModel represents MV, and
    /// GameViewScene represents V.
    fileprivate var gameViewModel: GameViewModel?

    /// Just for display evaluation
    fileprivate var evaluator: Evaluator?
    fileprivate var blackEval: Double = 0.0
    fileprivate var whiteEval: Double = 0.0
    fileprivate var debugString: String = ""

    // Misc
    var challengeLevelId: Int = -1

    init() {
    }

    func initialize(_ gameViewModel: GameViewModel, gameSettings: GameSettings) {
        blackPlayer = getPlayerByLevel(gameSettings.blackPlayerComputer, levelId: gameSettings.blackPlayerComputerLevelId, color: Pieces.black)
        whitePlayer = getPlayerByLevel(gameSettings.whitePlayerComputer, levelId: gameSettings.whitePlayerComputerLevelId, color: Pieces.white)

        hUI = HumanUserInput(gameManager: self)

        let b = SimpleBitBoard()
        b.initialize(8, height: 8)
        boardMediator = BoardMediator(board:b)
        boardMediator?.initializeBoard()

        self.gameViewModel = gameViewModel

        blackPlayer?.initialize(gameSettings.blackPlayerComputerLevelId)
        whitePlayer?.initialize(gameSettings.whitePlayerComputerLevelId)

        turn = Pieces.black

        switch (challengeModeComputer()) {
        case Pieces.black:
            challengeLevelId = gameSettings.blackPlayerComputerLevelId
        case Pieces.white:
            challengeLevelId = gameSettings.whitePlayerComputerLevelId
        default:
            NSLog("Not challenge mode")
        }


        let z = ZonesFactory().createZoneTypical7(99, bVal: 0.6, cVal: 3, dVal: 3.5, eVal: 3.9, fVal: 4.3, gVal: 4.8)
        let ev = ClassicalEvaluator()
        ev.configure([3.0, 0.5], wEdge: [1.0, 1.0], wFixedPieces: [5.0, 3.0], wOpenness: [2.5, 3.5], wBoardEvaluation: [2.5, 5.0], zones: z)
        self.evaluator = ev
    }

    /**
        Challenge is the game where either of player is human, anod other play is computer.
    
        :return: True if it's in challenge mode, false otherwise.
    */
    func isChallengeMode() -> Bool {
        let blackPlayerAndHuman = blackPlayer != nil && !blackPlayer!.isComputerPlayer()
        let whitePlayerAndHuman = whitePlayer != nil && !whitePlayer!.isComputerPlayer()

        return (blackPlayerAndHuman || whitePlayerAndHuman) && !(blackPlayerAndHuman && whitePlayerAndHuman)
    }

    func challengeModeComputer() -> Pieces {
        if isChallengeMode() {
            let blackPlayerAndHuman = blackPlayer != nil && !blackPlayer!.isComputerPlayer()
            if blackPlayerAndHuman {
                return Pieces.white
            } else {
                return Pieces.black
            }
        } else {
            return Pieces.none
        }
    }

    /**
        Get player object by level.
    
        :param: isComputer Always returns humand player if false, otherwise returns appopriate computer player.
        :param: levelId to look up Player for the given level
        :param: color Black/White to set to player
        ;return: Player object for the given level
    */
    fileprivate func getPlayerByLevel(_ isComputer: Bool, levelId: Int, color: Pieces) -> Player {
        let playerMediator = PlayerMediator(gameManager: self)
        if(isComputer) {
            let lc: LevelController = LevelController()

            if let player = lc.getPlayerByLevelId(levelId, playerMediator: playerMediator, color: color) {
                return player
            } else {
                let rcp = RandomPlayerWithEvaluation(playerMediator: playerMediator, color: color)

                let computerWeakestPlayer = rcp
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
        case Pieces.black:
            blackEval = eval!
            blackPlayer?.play()
        case Pieces.white:
            whiteEval = eval!
            whitePlayer?.play()
        default:
            assertionFailure("Should not reach this code!")
        }
    }

    /**
        Put a piece on the board.
    
        :param: color Color of piece to put
        :param: x x coordination
        :param: y y coordination
        :return: True if succeeds, false otherwise.
    */
    func put(_ color: Pieces, x: Int, y: Int) -> Bool {
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
                    Thread.sleep(forTimeInterval: 5/1000)
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
            var result = Pieces.none
            if(nBlack == nWhite) {
                result = Pieces.none
            } else if (nBlack > nWhite) {
                result = Pieces.black
            } else {
                result = Pieces.white
            }

            let message = "Black \(nBlack!) vs. White \(nWhite!)"
            var title = ""
            var showNext = false
            var nextExists = false
            var nextLabelText = ""
            // Is it challenge mode?
            let blackPlayerAndHuman = blackPlayer != nil && !blackPlayer!.isComputerPlayer()
//            let whitePlayerAndHuman = whitePlayer != nil && !whitePlayer!.isComputerPlayer()
            if(isChallengeMode()) {
                let lc: LevelController = LevelController()
                if let nextLevel = lc.getNextLevel(challengeLevelId) {
                    nextLabelText = "Next level: \(nextLevel.level)"
                    nextExists = true
                }
                // If challenge mode, human won
                switch(result) {
                case Pieces.none:
                    title = "Draw."
                case Pieces.black:
                    if(blackPlayerAndHuman) {
                        title = "You won."
                        openNextLevel()
                        showNext = true && nextExists
                    } else {
                        title = "You lose."
                    }
                case Pieces.white:
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
                case Pieces.black:
                    title = "Black won."
                case Pieces.white:
                    title = "White won."
                case Pieces.none:
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

    fileprivate func openNextLevel() {
        let lc: LevelController = LevelController()
        let nextChallengeLevel = lc.getNextLevel(challengeLevelId)
        if let uNCL = nextChallengeLevel {
            let gs: GameSettings = GameSettings()
            gs.loadFromUserDefaults()
            gs.difficultyHighestBeaten = uNCL.levelId
            gs.saveToUserDefaults()
        }
    }

    fileprivate func isCurrentTurnDoablePut() -> Bool {
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

    func doHumanPut(_ x: Int, y: Int) -> Bool {
        if(!isCurrentTurnHuman()) {
            NSLog("Not human turn currently. Ignoring")
            return false
        }

        switch (turn) {
        case .black:
            blackPlayer?.put(x, y: y)
        case .white:
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

    func isHumanTurn(_ turn: Pieces) -> Bool {
        let blackPlayerAndHuman = turn == Pieces.black && blackPlayer != nil && !blackPlayer!.isComputerPlayer()
        let whitePlayerAndHuman = turn == Pieces.white && whitePlayer != nil && !whitePlayer!.isComputerPlayer()
        return (blackPlayerAndHuman) || (whitePlayerAndHuman)
    }

    func setDebugString(_ string: String) {
        self.debugString = string
    }

    fileprivate func nextTurn(_ color: Pieces) -> Pieces {
        var s : Pieces = .black
        switch color {
        case .black:
            s = .white
        case .white:
            s = .black
        case .none:
            s = .black
        default:
            s = .black
        }
        return s
    }
}
