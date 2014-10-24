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

    private(set) var turn: Pieces = Pieces.None

    init() {
        
    }

    func initialize(gameViewModel: GameViewModel, gameSettings: GameSettings) {
        blackPlayer = getPlayerByLevel(gameSettings.blackPlayerComputer, color: Pieces.Black)
        whitePlayer = getPlayerByLevel(gameSettings.whitePlayerComputer, color: Pieces.White)

        hUI = HumanUserInput(gameManager: self)

        boardMediator = BoardMediator()
        boardMediator?.initializeBoard()

        self.gameViewModel = gameViewModel

        blackPlayer?.initialize(gameSettings.blackPlayerComputer)
        whitePlayer?.initialize(gameSettings.whitePlayerComputer)

        turn = Pieces.Black
    }

    private func getPlayerByLevel(level: Int, color: Pieces) -> Player {
        var playerMediator = PlayerMediator(gameManager: self)
        switch (level) {
        case 0:
            let humanPlayer = HumanPlayer(playerMediator: playerMediator, color: color)
            return humanPlayer
        default:
            let computerWeakestPlayer = RandomComputerPlayer(playerMediator: playerMediator, color: color)
            return computerWeakestPlayer
        }
    }

    func configure() {

    }

    // Main game loop logic
    func startGame() {
        NSLog("Game start")
        // Start
        switch (self.turn) {
        case Pieces.Black:
            NSLog("Black turn start")
            blackPlayer?.play()
        case Pieces.White:
            NSLog("White turn start")
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
            return false
        }

        let changes = self.boardMediator?.put(color, x: x, y: y)
        self.turn = nextTurn(self.turn)
        NSLog("Put \(x), \(y)")
        if let unwrappedChanges = changes {
            self.gameViewModel?.update(unwrappedChanges, put: [(x, y)], showPuttables: isCurrentTurnHuman())
        }

        if(isGameOver()) {
            // Game Over, show result
            NSLog("Game Over")
        } else if (!isCurrentTurnDoablePut()) {
            NSLog("Current player cannot do anything, skipping")
            // Current player cannot do anything. Skip
            self.turn = nextTurn(self.turn)
            self.gameViewModel?.update([], put: [], showPuttables: isCurrentTurnHuman())
            self.gameViewModel?.showPasses()
            startGame()
        } else {
            // Then next player's turn
            NSLog("Next turn")
            startGame()
        }

        // If returned true, current player can stop thinking
        // hand
        return true
    }

    private func isGameOver() -> Bool {
        if let unwrappedBoardMediator = boardMediator {
            // If no action from both side, game over
            let puttablesFromCurrentTurn = unwrappedBoardMediator.getPuttables(turn)
            let puttablesFromNextTurn = unwrappedBoardMediator.getPuttables(nextTurn(turn))
            NSLog("game over check -- current : \(puttablesFromCurrentTurn.count), next : \(puttablesFromNextTurn.count)")
            if(puttablesFromCurrentTurn.count == 0 && puttablesFromNextTurn.count == 0) {
                return true
            }
        }
        return false
    }

    private func isCurrentTurnDoablePut() -> Bool {
        if let unwrappedBoardMediator = boardMediator {
            // If can puttable position is more than 0, doable action
            let puttablesFromCurrentTurn = unwrappedBoardMediator.getPuttables(turn)
            NSLog("next player's puttables are \(puttablesFromCurrentTurn)")
            if(puttablesFromCurrentTurn.count > 0) {
                return true
            }
        }
        return false
    }

    func getBoardRepresentative() -> BoardRepresentative? {
        if let unwrappedBoardMediator = self.boardMediator {
            let boardRepresentative = BoardRepresentative(boardMediator: unwrappedBoardMediator)
            return boardRepresentative
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
        let blackPlayerAndHuman = turn == Pieces.Black && blackPlayer != nil && !blackPlayer!.isComputerPlayer()
        let whitePlayerAndHuman = turn == Pieces.White && whitePlayer != nil && !whitePlayer!.isComputerPlayer()
        return (blackPlayerAndHuman) || (whitePlayerAndHuman)
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