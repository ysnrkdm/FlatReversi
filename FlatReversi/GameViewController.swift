//
//  GameViewController.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/11/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import SpriteKit

extension SKNode {
    class func unarchiveFromFile(file : NSString) -> SKNode? {
        if let path = NSBundle.mainBundle().pathForResource(file, ofType: "sks") {
            var sceneData = NSData(contentsOfFile: path, options: .DataReadingMappedIfSafe, error: nil)
            var archiver = NSKeyedUnarchiver(forReadingWithData: sceneData!)
            
            archiver.setClass(self.classForKeyedUnarchiver(), forClassName: "SKScene")
            let scene = archiver.decodeObjectForKey(NSKeyedArchiveRootObjectKey) as GameScene
            archiver.finishDecoding()
            return scene
        } else {
            return nil
        }
    }
}

class GameViewController: UIViewController, UINavigationBarDelegate {

    @IBOutlet weak var navbarTitle: UINavigationItem!
    @IBOutlet weak var navbar: UINavigationBar!
    weak var thisScene: GameScene?

    @IBOutlet weak var skView: SKView!

    override func viewDidLoad() {
        super.viewDidLoad()

        self.view.sizeToFit()

        if let scene = GameScene.unarchiveFromFile("GameScene") as? GameScene {
            // Configure the view.
//            let skView = self.view as SKView

            scene.tintColor = self.view.tintColor
            skView.showsFPS = true
            skView.showsNodeCount = true

            /* Sprite Kit applies additional optimizations to improve rendering performance */
            skView.ignoresSiblingOrder = true
            
            /* Set the scale mode to scale to fit the window */
            scene.scaleMode = .AspectFill
            NSLog("UIApplication().statusBarFrame origin y = \(UIApplication.sharedApplication().statusBarFrame.origin.y)")
            NSLog("UIApplication().statusBarFrame size y = \(UIApplication.sharedApplication().statusBarFrame.size.height)")
            NSLog("skView.bounds.origin.y = \(skView.bounds.origin.y)")
            NSLog("navbar.bounds.origin.y = \(navbar.bounds.origin.y)")
            NSLog("navbar.bounds.height = \(navbar.bounds.height)")
            scene.topYOffset = UIApplication.sharedApplication().statusBarFrame.origin.y + UIApplication.sharedApplication().statusBarFrame.size.height + navbar.bounds.origin.y + navbar.bounds.height
            
            skView.presentScene(scene)
            thisScene = scene
        }

        updateNavBarTitle("Reversi")

        navbar.delegate = self;
        if (floor(NSFoundationVersionNumber) > NSFoundationVersionNumber_iOS_6_1) {
            navbar.barTintColor = UIColor.whiteColor();
        }
    }

    func positionForBar(bar: UIBarPositioning!) -> UIBarPosition {
        return UIBarPosition.TopAttached
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)

        NSLog("size before %f - %f", self.view.frame.width.native, navbar.frame.size.width.native)

        NSLog("size %f - %f", self.view.frame.width.native, navbar.frame.size.width.native)

        let skView = self.view as SKView
        skView.paused = false
    }

    override func shouldAutorotate() -> Bool {
        return false
    }

    override func supportedInterfaceOrientations() -> Int {
        if UIDevice.currentDevice().userInterfaceIdiom == .Phone {
            return Int(UIInterfaceOrientationMask.AllButUpsideDown.rawValue)
        } else {
            return Int(UIInterfaceOrientationMask.All.rawValue)
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Release any cached data, images, etc that aren't in use.
    }

    override func prefersStatusBarHidden() -> Bool {
        return false
    }
    @IBAction func push(sender: AnyObject) {
        let skView = self.view as SKView
        skView.paused = true
        NSLog("pressed Settings")

        performSegueWithIdentifier("settings",sender: nil)
    }
    @IBAction func playNewGame(sender: AnyObject) {
        NSLog("play new game!")
        thisScene?.startGame()
    }

    func updateNavBarTitle(str: String) {
        navbarTitle.title = str
    }
}
