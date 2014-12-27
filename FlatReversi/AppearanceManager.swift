//
//  AppearanceManager.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import SpriteKit

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

struct ColorPalette {
    var uiColorBackground: UIColor
    var uiColorBackground2: UIColor
    var uiColorTint: UIColor
    var uiColorText: UIColor

    var boardFontColor: SKColor
    var boardGridColor: SKColor

    var boardBackgroundColor: SKColor

    var boardBlackPieceFillColor: SKColor
    var boardBlackPieceStrokeColor: SKColor
    var boardWhitePieceFillColor: SKColor
    var boardWhitePieceStrokeColor: SKColor
    var boardGuidePieceFillColor: SKColor

    var boardPopupFillColor: SKColor
    var boardPopupFontColor: SKColor
}

class AppearanceManager {
    class func applyWhiteGrayTheme() {
        applyColorPalette(getColorPalette(.WhiteGray))
        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.Default, animated: true)
        persist(.WhiteGray)
    }

    class func applyLikeDarculaTheme() {
        applyColorPalette(getColorPalette(.LikeDarcula))
        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.LightContent, animated: true)
        persist(.LikeDarcula)
    }

    class func getColorPalette(appearance: Appearance) -> ColorPalette {
        switch appearance {
        case .LikeDarcula:
            var darcula = ColorPalette(
                uiColorBackground: UIColor(red: 57/255, green: 57/255, blue: 57/255, alpha: 1),
                uiColorBackground2: UIColor(red: 73/255, green: 76/255, blue: 78/255, alpha: 1),
                uiColorTint: UIColor(red: 216/255, green: 137/255, blue: 32/255, alpha: 1),
                uiColorText: UIColor.whiteColor(),
                boardFontColor: SKColor(red: 0.8, green: 0.8, blue: 0.8, alpha: 1),
                boardGridColor: SKColor(red: 0.6, green: 0.6, blue: 0.6, alpha: 1),
                boardBackgroundColor: UIColor(red: 73/255, green: 76/255, blue: 78/255, alpha: 1),
                boardBlackPieceFillColor: SKColor(red: 0.1, green: 0.1, blue: 0.1, alpha: 1),
                boardBlackPieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardWhitePieceFillColor: SKColor(red: 1, green: 1, blue: 1, alpha: 1),
                boardWhitePieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardGuidePieceFillColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 0.5),
                boardPopupFillColor: SKColor(red: 0.3, green: 0.3, blue: 0.3, alpha: 0.5),
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0))
            return darcula
        case .WhiteGray:
            var whitegray = ColorPalette(
                uiColorBackground: UIColor.whiteColor(),
                uiColorBackground2: UIColor(red: 239/255, green: 239/255, blue: 244/255, alpha: 1),
                uiColorTint: UIColor(red: 0, green: 122/255, blue: 1, alpha: 1),
                uiColorText: UIColor(red: 0.05, green: 0.05, blue: 0.05, alpha: 1),
                boardFontColor: SKColor(red: 0.1, green: 0.1, blue: 0.1, alpha: 1),
                boardGridColor: SKColor(red: 0.75, green: 0.75, blue: 0.75, alpha: 1),
                boardBackgroundColor: SKColor(red: 245/255, green: 245/255, blue: 245/255, alpha: 1),
                boardBlackPieceFillColor: SKColor(red: 0.1, green: 0.1, blue: 0.1, alpha: 1),
                boardBlackPieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardWhitePieceFillColor: SKColor(red: 1, green: 1, blue: 1, alpha: 1),
                boardWhitePieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardGuidePieceFillColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 0.5),
                boardPopupFillColor: SKColor(red: 0.3, green: 0.3, blue: 0.3, alpha: 0.5),
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0))
            return whitegray
        }
    }

    class func applyColorPalette(colorPalette: ColorPalette) {
        UINavigationBar.appearance().tintColor = colorPalette.uiColorTint
        UINavigationBar.appearance().backgroundColor = colorPalette.uiColorBackground
        UINavigationBar.appearance().barTintColor = colorPalette.uiColorBackground
        let titleDict: NSDictionary = [NSForegroundColorAttributeName: colorPalette.uiColorText]
        UINavigationBar.appearance().titleTextAttributes = titleDict

        UITableView.appearance().tintColor = colorPalette.uiColorTint
        UITableView.appearance().backgroundColor = colorPalette.uiColorBackground2
        UITableViewCell.appearance().tintColor = colorPalette.uiColorTint
        UITableViewCell.appearance().backgroundColor = colorPalette.uiColorBackground

        UILabel.appearance().textColor = colorPalette.uiColorText

        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.LightContent, animated: true)
        
        resetViews()
    }

    class func persist(appearanceToSet: Appearance) {
        var gc = GameSettings()
        gc.loadFromUserDefaults()
        gc.appearance = appearanceToSet
        gc.saveToUserDefaults()
    }

    class func load() -> ColorPalette {
        var gc = GameSettings()
        gc.loadFromUserDefaults()

        switch gc.appearance {
        case .WhiteGray:
            applyWhiteGrayTheme()
            return getColorPalette(.WhiteGray)
        case .LikeDarcula:
            applyLikeDarculaTheme()
            return getColorPalette(.LikeDarcula)
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
