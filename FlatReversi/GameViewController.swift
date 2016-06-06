//
//  GameViewController.swift
//  MyFirstSpriteKit
//
//  Created by Kodama Yoshinori on 10/11/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import SpriteKit
import GoogleMobileAds

extension SKNode {
    class func unarchiveFromFile(file : NSString) -> SKNode? {
        if let path = NSBundle.mainBundle().pathForResource(file as String, ofType: "sks") {
            guard let sceneData = try? NSData(contentsOfFile: path, options: .DataReadingMappedIfSafe) else {
                return nil
            }
            let archiver = NSKeyedUnarchiver(forReadingWithData: sceneData)
            archiver.setClass(self.classForKeyedUnarchiver(), forClassName: "SKScene")
            let scene = archiver.decodeObjectForKey(NSKeyedArchiveRootObjectKey) as! GameScene
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

    var currentScene: SKScene? = nil

    var bannerView: GADBannerView?

    override func viewDidLoad() {
        super.viewDidLoad()

        self.view.sizeToFit()

        if let scene = GameScene.unarchiveFromFile("GameScene") as? GameScene {
            // Configure the view.

            currentScene = scene
            scene.tintColor = self.view.tintColor
            skView.showsFPS = false
            skView.showsNodeCount = false

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
            scene.statusBarHeight = UIApplication.sharedApplication().statusBarFrame.size.height
            
            skView.presentScene(scene)
            thisScene = scene
        }

        updateNavBarTitle("Reversi")

        navbar.delegate = self;
        if (floor(NSFoundationVersionNumber) > NSFoundationVersionNumber_iOS_6_1) {
            navbar.barTintColor = UIColor.whiteColor();
        }

        let gs: GameSettings = GameSettings()
        gs.loadFromUserDefaults()
        if !gs.validate() {
            gs.resetAndSave()
        }

        // Ad
        let origin = CGPointMake(0.0, self.view.frame.size.height - CGSizeFromGADAdSize(kGADAdSizeBanner).height);
        bannerView = GADBannerView(adSize: kGADAdSizeBanner, origin: origin)
        if let uBannerView = bannerView {
            uBannerView.adUnitID = "ca-app-pub-4004659206753296/7384819767"
//            uBannerView.delegate = self
            uBannerView.rootViewController = self
//            self.view.addSubview(uBannerView)
            uBannerView.loadRequest(GADRequest())
        }
    }

    func positionForBar(bar: UIBarPositioning) -> UIBarPosition {
        return UIBarPosition.TopAttached
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)

        NSLog("size before %f - %f", self.view.frame.width.native, navbar.frame.size.width.native)

        NSLog("size %f - %f", self.view.frame.width.native, navbar.frame.size.width.native)

        let sView = self.view as! SKView
        sView.paused = false
        skView.paused = false
        currentScene?.paused = false

//        AppearanceManager.load()
//        AppearanceManager.resetViews()

        thisScene?.loadColors()
    }

    override func viewWillDisappear(animated: Bool) {
        let sView = self.view as! SKView
        currentScene?.paused = true
        sView.paused = true
        skView.paused = true

    }

    override func shouldAutorotate() -> Bool {
        return false
    }

    override func supportedInterfaceOrientations() -> UIInterfaceOrientationMask {
        if UIDevice.currentDevice().userInterfaceIdiom == .Phone {
            return UIInterfaceOrientationMask.AllButUpsideDown
        } else {
            return UIInterfaceOrientationMask.All
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
        let sView = self.view as! SKView
        currentScene?.paused = true
        sView.paused = true
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

    func popupAlert(title: String, message: String, actions: [UIAlertAction]) {
        let alertController = UIAlertController(title: title, message: message, preferredStyle: .ActionSheet)
        for action in actions {
            alertController.addAction(action)
        }
        presentViewController(alertController, animated: true, completion: nil)
    }
}
