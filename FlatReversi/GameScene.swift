//
//  GameScene.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/11/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import SpriteKit

class GameScene: SKScene {
    var blackComputerLevel: Int = 0
    var whiteComputerLevel: Int = 1

    var gameSettings = GameSettings()
    var gameManager = GameManager()
    var gameViewModel: GameViewModel?

    var boardSprites: [[SKShapeNode?]] = []
    var lastPut: SKShapeNode? = nil

    var showGuides = true

    var time:CFTimeInterval = 0
    var timeCounterOn = false
    var lastUpdatedTime:CFTimeInterval = 0

    var topYOffset: CGFloat = 0

    var tintColor: UIColor = UIColor(red: 0, green: 122/255, blue: 1, alpha: 1)

    let lock = NSLock()

    var drawCount = 0

    override func didMoveToView(view: SKView) {
        /* Setup your scene here */

        let boardView = self.childNodeWithName("Board") as SKSpriteNode
        var width : CGFloat = boardView.size.width
        var height : CGFloat = boardView.size.height
        var piece_width = width / 8
        var line_width : CGFloat = 2.0
        var color = SKColor(red: 0.6, green: 0.6, blue: 0.6, alpha: 0.6)

        topYOffset += piece_width

        // Horizontal lines
        for var by : CGFloat = 0; by <= width; by += piece_width {
            let sprite = SKShapeNode(rect: screenRectFromVritualRect(CGRectMake(0, by, width, line_width)))
            sprite.fillColor = color
            boardView.addChild(sprite)
        }
        // Vertical lines
        for var bx : CGFloat = 0; bx <= width; bx += piece_width {
            let rect = screenRectFromVritualRect(CGRectMake(bx, width, line_width, width))
            let sprite = SKShapeNode(rect: rect)
            sprite.fillColor = color
            boardView.addChild(sprite)
        }

        gameSettings.whitePlayerComputer = 1
        startGame()
        NSLog("\n" + gameManager.toString())
    }

    private func clearPieces() {
        for row in boardSprites {
            for cell in row {
                cell?.removeFromParent()
            }
        }

        boardSprites = []
        for var y = 0; y < gameManager.boardMediator?.height(); ++y {
            boardSprites.append([nil, nil, nil, nil, nil, nil, nil, nil])
        }

        lastPut?.removeFromParent()
        lastPut = nil
    }

    func startGame() {
        if let unwrappedGVM = gameViewModel {
            gameManager.initialize(unwrappedGVM, gameSettings: gameSettings)
        } else {
            gameViewModel = GameViewModel(gameManager: gameManager, view: self)
            gameManager.initialize(gameViewModel!, gameSettings: gameSettings)
        }
        clearPieces()
        addChildrenFromBoard(gameManager.boardMediator!, changes: [], put: [], showPuttables: gameManager.isCurrentTurnHuman())
        time = 0
        lastUpdatedTime = 0
        timeCounterOn = false
        gameManager.startGame()
    }

    override func touchesBegan(touches: NSSet, withEvent event: UIEvent) {
        /* Called when a touch begins */
        
        for touch: AnyObject in touches {
                let boardView = self.childNodeWithName("Board") as SKSpriteNode
                var width : CGFloat = boardView.size.width
                let location_boardView = touch.locationInNode(boardView)
                var piece_width = width / 8
                let (ex, ey) = coordFromTouch(location_boardView.x, y: location_boardView.y, piece_width: piece_width)
            NSLog("Touched \(ex), \(ey)")

            if let hUI = gameManager.getHumanUserInput() {
                hUI.put(ex, y: ey)
            }
        }
    }

    private func coordFromTouch(x: CGFloat, y: CGFloat, piece_width: CGFloat) -> (Int, Int) {
        var pos_x = Int(floor(virtualXFromScreenX(x) / piece_width))
        var pos_y = Int(floor(virtualYFromScreenY(y) / piece_width))
        NSLog("%f, %f -> %f, %f", x.native, y.native, virtualXFromScreenX(x).native, virtualYFromScreenY(y).native)

        return (pos_x, pos_y)
    }
    
    func updateView(bd : BoardMediator, changes:[(Int, Int)], put: [(Int, Int)], showPuttables: Bool) {
        addChildrenFromBoard(bd, changes: changes, put: put, showPuttables: showPuttables)
    }

    func addChildrenFromBoard(bd : BoardMediator, changes:[(Int, Int)], put: [(Int, Int)], showPuttables: Bool) {
        synchronized(lock) {
            let sleepMS = 0.5
            for c in 0..<Int(2/0.5) {
                if(self.drawCount == 0) {
                    NSLog("Finished waiting drawing")
                    break
                }
                NSThread.sleepForTimeInterval(sleepMS)
                NSLog("Waiting for finish drawing...")
            }
            self.drawCount = 0
            for y in 0..<bd.height() {
                for x in 0..<bd.width() {
                    let turn = bd.get(x, y: y)

                    if(turn == Pieces.Empty) {
                        var sp = self.boardSprites[y][x]
                        sp?.removeFromParent()
                        self.boardSprites[y][x] = nil
                        continue
                    }

                    let boardView = self.childNodeWithName("Board") as SKSpriteNode
                    var width : CGFloat = boardView.size.width
                    var radius = width / 8 / 2 * 0.8

                    var flipAnimation = false
                    var putAnimation = false
                    var sprite = self.boardSprites[y][x]
                    if(sprite == nil && (turn == Pieces.Black || turn == Pieces.White)) {
                        sprite = SKShapeNode(circleOfRadius: radius)
                        boardView.addChild(sprite!)
                    } else if(turn == Pieces.Guide) {
                        // Erase current sprite
                        sprite?.removeFromParent()
                        self.boardSprites[y][x] = nil
                        // And update with guide
                        if(showPuttables) {
                            sprite = SKShapeNode(circleOfRadius: radius / 4)
                            self.boardSprites[y][x] = sprite
                            boardView.addChild(sprite!)
                        } else {
                            continue
                        }
                    } else if(turn == Pieces.Black || turn == Pieces.White) {
                        for (cx, cy) in changes {
                            if(cx == x && cy == y) {
                                flipAnimation = true
                                break
                            }
                        }
                        if(put.count > 0 && put[0].0 == x && put[0].1 == y) {
                            // Delete Guide
                            sprite?.removeFromParent()
                            self.boardSprites[y][x] = nil
                            // Put piece
                            sprite = SKShapeNode(circleOfRadius: radius)
                            boardView.addChild(sprite!)
                            putAnimation = true
                        }
                    }

                    if let spriteUnwrapped = sprite {

                        var colorTo = SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 0.5)
                        if(turn == Pieces.White) {
                            colorTo = SKColor(red: 1, green: 1, blue: 1, alpha: 1)
                        } else if (turn == Pieces.Black) {
                            colorTo = SKColor(red: 0.1, green: 0.1, blue: 0.1, alpha: 1)
                        } else if (turn == Pieces.Guide && self.showGuides) {
                            colorTo = SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 0.5)
                        } else {
                            assertionFailure("Should not reach this code!")
                        }

                        self.drawCount+=1
                        let reduceDrawCount = SKAction.runBlock({() in self.drawCount-=1})
                        if(putAnimation) {
                            let fadeOut = SKAction.fadeAlphaTo(1.0, duration: 0.05)
                            let colorChange = SKAction.runBlock({() in spriteUnwrapped.strokeColor = self.tintColor})
                            let fadeIn = SKAction.fadeAlphaTo(1.0, duration: 0.05)
                            let fadeOut2 = SKAction.fadeAlphaTo(0.0, duration: 0.1)
                            let colorChange2 = SKAction.runBlock({() in spriteUnwrapped.fillColor = colorTo; spriteUnwrapped.strokeColor = SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1)})
                            let fadeIn2 = SKAction.fadeAlphaTo(1.0, duration: 0.1)


                            let sequenceActions = SKAction.sequence([
                                fadeOut, colorChange, fadeIn,
                                fadeOut2, colorChange2, fadeIn2,
                                reduceDrawCount,
                            ])

                            spriteUnwrapped.runAction(sequenceActions, completion: {() in spriteUnwrapped.fillColor = colorTo; spriteUnwrapped.alpha = 1.0})

                        } else if(flipAnimation) {
                            let fadeOut = SKAction.fadeAlphaTo(0.0, duration: 0.2)
                            let colorChange = SKAction.runBlock({() in spriteUnwrapped.fillColor = colorTo})
                            let fadeIn = SKAction.fadeAlphaTo(1.0, duration: 0.2)

                            let sequenceActions = SKAction.sequence([fadeOut, colorChange, fadeIn, reduceDrawCount])

                            spriteUnwrapped.runAction(sequenceActions, completion: {() in spriteUnwrapped.fillColor = colorTo; spriteUnwrapped.alpha = 1.0})
                        } else {
                            spriteUnwrapped.fillColor = colorTo
                            self.drawCount-=1
                        }
                        spriteUnwrapped.strokeColor = SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1)
                        spriteUnwrapped.lineWidth = 2

                        // Align the piece to boardView
                        var piece_width = width / 8
                        var px = CGFloat(x) * piece_width + piece_width / 2
                        var py = (CGFloat(y) * piece_width) + piece_width / 2
                        spriteUnwrapped.position = self.screenPointFromVirtualPoint(CGPointMake(px, py))

                        // Add
                        self.boardSprites[y][x] = spriteUnwrapped
                    } // if let spriteUnwrapped = sprite
                } // for x
            } // for y

            // Hilight put position
            if(put.count > 0) {
                let (x, y) = put[0]
                let boardView = self.childNodeWithName("Board") as SKSpriteNode
                var piece_width = boardView.size.width / 8
                self.lastPut?.removeFromParent()
                self.lastPut = SKShapeNode(rect: self.screenRectFromVritualRect(CGRectMake(CGFloat(x) * piece_width, CGFloat(y+1) * piece_width, piece_width, piece_width)))
                self.lastPut?.strokeColor = self.tintColor
                boardView.addChild(self.lastPut!)
            }
        } // synchronized
    }

    private func screenXFromVirtualX(x: CGFloat) -> CGFloat {
        return x - 320
    }

    private func screenYFromVirtualY(y: CGFloat) -> CGFloat {
        return 1136 - y - topYOffset
    }

    private func virtualXFromScreenX(x: CGFloat) -> CGFloat {
        return x + 320
    }

    private func virtualYFromScreenY(y: CGFloat) -> CGFloat {
        return 1136 - y - topYOffset
    }

    private func screenPointFromVirtualPoint(point: CGPoint) -> CGPoint {
        return CGPointMake(screenXFromVirtualX(point.x), screenYFromVirtualY(point.y))
    }

    private func screenRectFromVritualRect(rect: CGRect) -> CGRect {
        return CGRectMake(screenXFromVirtualX(rect.origin.x), screenYFromVirtualY(rect.origin.y), rect.width, rect.height)
    }

    override func update(currentTime: CFTimeInterval) {
        if(lastUpdatedTime == 0) {
            lastUpdatedTime = currentTime
        }

        /* Called before each frame is rendered */
        if(timeCounterOn) {
            let viewController = self.view?.window?.rootViewController as GameViewController

            time += currentTime - lastUpdatedTime

            var turnStr = ""
            switch gameManager.turn {
            case Pieces.Black:
                turnStr = "Black turn"
            case Pieces.White:
                turnStr = "White turn"
            default:
                turnStr = "*"
            }

            var title = "\(turnStr)"
            viewController.updateNavBarTitle(title)
        }
    }
}
