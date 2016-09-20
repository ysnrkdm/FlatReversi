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
    class func unarchiveFromFile(_ file : NSString) -> SKNode? {
        if let path = Bundle.main.path(forResource: file as String, ofType: "sks") {
            guard let sceneData = try? Data(contentsOf: URL(fileURLWithPath: path), options: .mappedIfSafe) else {
                return nil
            }
            let archiver = NSKeyedUnarchiver(forReadingWith: sceneData)
            archiver.setClass(self.classForKeyedUnarchiver(), forClassName: "SKScene")
            let scene = archiver.decodeObject(forKey: NSKeyedArchiveRootObjectKey) as! GameScene
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

    @IBOutlet weak var bannerView: GADBannerView!

    override func viewDidLoad() {
        super.viewDidLoad()

        self.view.sizeToFit()

        if let scene = GameScene.unarchiveFromFile("GameScene") as? GameScene {
            currentScene = scene
            scene.tintColor = self.view.tintColor
            skView.showsFPS = false
            skView.showsNodeCount = false

            /* Sprite Kit applies additional optimizations to improve rendering performance */
            skView.ignoresSiblingOrder = true
            
            /* Set the scale mode to scale to fit the window */
            scene.scaleMode = .aspectFill
            NSLog("UIApplication().statusBarFrame origin y = \(UIApplication.shared.statusBarFrame.origin.y)")
            NSLog("UIApplication().statusBarFrame size y = \(UIApplication.shared.statusBarFrame.size.height)")
            NSLog("skView.bounds.origin.y = \(skView.bounds.origin.y)")
            NSLog("navbar.bounds.origin.y = \(navbar.bounds.origin.y)")
            NSLog("navbar.bounds.height = \(navbar.bounds.height)")
            scene.topYOffset = UIApplication.shared.statusBarFrame.origin.y + UIApplication.shared.statusBarFrame.size.height + navbar.bounds.origin.y + navbar.bounds.height
            scene.topYOffset = 100
            scene.statusBarHeight = UIApplication.shared.statusBarFrame.size.height
            
            skView.presentScene(scene)
            thisScene = scene
        }

        updateNavBarTitle("Reversi")

        navbar.delegate = self;
        if (floor(NSFoundationVersionNumber) > NSFoundationVersionNumber_iOS_6_1) {
            navbar.barTintColor = UIColor.white;
        }

        let gs: GameSettings = GameSettings()
        gs.loadFromUserDefaults()
        if !gs.validate() {
            gs.resetAndSave()
        }

        // Ad
        if let uBannerView = bannerView {
            uBannerView.adUnitID = "ca-app-pub-4004659206753296/7384819767"
            uBannerView.rootViewController = self
            uBannerView.load(GADRequest())
        }
    }

    func position(for bar: UIBarPositioning) -> UIBarPosition {
        return UIBarPosition.topAttached
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)

        NSLog("size before %f - %f", self.view.frame.width.native, navbar.frame.size.width.native)
        NSLog("size %f - %f", self.view.frame.width.native, navbar.frame.size.width.native)

        let sView = self.view as! SKView
        sView.isPaused = false
        skView.isPaused = false
        currentScene?.isPaused = false

        thisScene?.loadColors()
    }

    override func viewWillDisappear(_ animated: Bool) {
        let sView = self.view as! SKView
        currentScene?.isPaused = true
        sView.isPaused = true
        skView.isPaused = true

    }

    override var shouldAutorotate : Bool {
        return false
    }

    override var supportedInterfaceOrientations : UIInterfaceOrientationMask {
        if UIDevice.current.userInterfaceIdiom == .phone {
            return UIInterfaceOrientationMask.allButUpsideDown
        } else {
            return UIInterfaceOrientationMask.all
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Release any cached data, images, etc that aren't in use.
    }

    override var prefersStatusBarHidden : Bool {
        return false
    }
    
    @IBAction func push(_ sender: AnyObject) {
        let sView = self.view as! SKView
        currentScene?.isPaused = true
        sView.isPaused = true
        skView.isPaused = true
        NSLog("pressed Settings")

        performSegue(withIdentifier: "settings",sender: nil)
    }

    @IBAction func playNewGame(_ sender: AnyObject) {
        NSLog("play new game!")
        thisScene?.startGame()
    }

    func updateNavBarTitle(_ str: String) {
        navbarTitle.title = str
    }

    func popupAlert(_ title: String, message: String, actions: [UIAlertAction]) {
        let alertController = UIAlertController(title: title, message: message, preferredStyle: .actionSheet)
        for action in actions {
            alertController.addAction(action)
        }
        present(alertController, animated: true, completion: nil)
    }
}
