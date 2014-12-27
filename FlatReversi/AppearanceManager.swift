//
//  AppearanceManager.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit

enum Appearance {
    case WhiteGray, LikeDarcula

    func toInteger() -> Int {
        switch self {
        case .WhiteGray:
            return 0
        case .LikeDarcula:
            return 1
        }
    }

    func fromInteger(num: Int) -> Appearance {
        switch num {
        case 0:
            return .WhiteGray
        case 1:
            return .LikeDarcula
        default:
            assertionFailure("Please specify correct enum for Appearance")
        }
    }
}

class AppearanceManager {
    class func applyWhiteGrayTheme() {
        let colorBackground = UIColor.whiteColor()
        let colorBackground2 = UIColor(red: 239/255, green: 239/255, blue: 244/255, alpha: 1)
        let colorTint = UIColor(red: 0, green: 122/255, blue: 1, alpha: 1)
        let colorText = UIColor(red: 0.05, green: 0.05, blue: 0.05, alpha: 1)

        UINavigationBar.appearance().tintColor = colorTint
        UINavigationBar.appearance().backgroundColor = colorBackground
        UINavigationBar.appearance().barTintColor = colorBackground
        let titleDict: NSDictionary = [NSForegroundColorAttributeName: colorText]
        UINavigationBar.appearance().titleTextAttributes = titleDict

        UITableView.appearance().tintColor = colorTint
        UITableView.appearance().backgroundColor = colorBackground2

        UITableViewCell.appearance().tintColor = colorTint
        UITableViewCell.appearance().backgroundColor = colorBackground

        UILabel.appearance().textColor = colorText

        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.Default, animated: true)

        resetViews()
        persist(.WhiteGray)
    }

    class func applyLikeDarculaTheme() {
        let colorBackground = UIColor(red: 57/255, green: 57/255, blue: 57/255, alpha: 1)
        let colorBackground2 = UIColor(red: 73/255, green: 76/255, blue: 78/255, alpha: 1)
        let colorTint = UIColor(red: 216/255, green: 137/255, blue: 32/255, alpha: 1)
        let colorText = UIColor.whiteColor()

        UINavigationBar.appearance().tintColor = colorTint
        UINavigationBar.appearance().backgroundColor = colorBackground
        UINavigationBar.appearance().barTintColor = colorBackground
        let titleDict: NSDictionary = [NSForegroundColorAttributeName: colorText]
        UINavigationBar.appearance().titleTextAttributes = titleDict

        UITableView.appearance().tintColor = colorTint
        UITableView.appearance().backgroundColor = colorBackground2
        UITableViewCell.appearance().tintColor = colorTint
        UITableViewCell.appearance().backgroundColor = colorBackground

        UILabel.appearance().textColor = colorText

        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.LightContent, animated: true)

        resetViews()
        persist(.LikeDarcula)
    }

    class func persist(appearanceToSet: Appearance) {
        var gc = GameSettings()
        gc.loadFromUserDefaults()
        gc.appearance = appearanceToSet
        gc.saveToUserDefaults()
    }

    class func load() {
        var gc = GameSettings()
        gc.loadFromUserDefaults()

        switch gc.appearance {
        case .WhiteGray:
            applyWhiteGrayTheme()
        case .LikeDarcula:
            applyLikeDarculaTheme()
        }
    }

    class func resetViews() {
        let windows = UIApplication.sharedApplication().windows as [UIWindow]
        for window in windows {
            let subviews = window.subviews as [UIView]
            for v in subviews {
                v.removeFromSuperview()
                window.addSubview(v)
            }
        }
    }
}
