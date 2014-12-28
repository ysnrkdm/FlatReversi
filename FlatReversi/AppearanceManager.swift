//
//  AppearanceManager.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import SpriteKit

enum Appearance: String {
    case WhiteGray = "WhiteGray", LikeDarcula = "LikeDarcula", Night = "Night", Classic = "Classic", ClassicDark = "ClassicDark"

    static func toList() -> [Appearance] {
        return [.WhiteGray, .LikeDarcula, .Night, .Classic, .ClassicDark]
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

    var uiStatusBarStyle: UIStatusBarStyle
}

class AppearanceManager {
//    class func applyWhiteGrayTheme() {
//        applyColorPalette(getColorPalette(.WhiteGray))
//        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.Default, animated: true)
//        persist(.WhiteGray)
//    }
//
//    class func applyLikeDarculaTheme() {
//        applyColorPalette(getColorPalette(.LikeDarcula))
//        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.LightContent, animated: true)
//        persist(.LikeDarcula)
//    }
//
//    class func applyNightTheme() {
//        applyColorPalette(getColorPalette(.Night))
//        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.LightContent, animated: true)
//        persist(.Night)
//    }
//
//    class func applyClassicTheme() {
//        applyColorPalette(getColorPalette(.Classic))
//        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.Default, animated: true)
//        persist(.Classic)
//    }
//
//    class func applyClassicDarkTheme() {
//        applyColorPalette(getColorPalette(.ClassicDark))
//        UIApplication.sharedApplication().setStatusBarStyle(UIStatusBarStyle.LightContent, animated: true)
//        persist(.ClassicDark)
//    }

    class func getColorPalette(appearance: Appearance) -> ColorPalette {
        switch appearance {
        case .LikeDarcula:
            var darcula = ColorPalette(
                uiColorBackground: UIColor(red: 52/255, green: 54/255, blue: 56/255, alpha: 1),
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
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0),
                uiStatusBarStyle: UIStatusBarStyle.LightContent
            )
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
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0),
                uiStatusBarStyle: UIStatusBarStyle.Default
            )
            return whitegray
        case .Night:
            var night = ColorPalette(
                uiColorBackground: UIColor(red: 50/255, green: 54/255, blue: 59/255, alpha: 1),
                uiColorBackground2: UIColor(red: 55/255, green: 59/255, blue: 64/255, alpha: 1),
                uiColorTint: UIColor(red: 255/255, green: 108/255, blue: 3/255, alpha: 1),
                uiColorText: UIColor.whiteColor(),
                boardFontColor: SKColor(red: 0.8, green: 0.8, blue: 0.8, alpha: 1),
                boardGridColor: SKColor(red: 0.6, green: 0.6, blue: 0.6, alpha: 1),
                boardBackgroundColor: UIColor(red: 55/255, green: 59/255, blue: 64/255, alpha: 1),
                boardBlackPieceFillColor: SKColor(red: 0.1, green: 0.1, blue: 0.1, alpha: 1),
                boardBlackPieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardWhitePieceFillColor: SKColor(red: 1, green: 1, blue: 1, alpha: 1),
                boardWhitePieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardGuidePieceFillColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 0.5),
                boardPopupFillColor: SKColor(red: 0.3, green: 0.3, blue: 0.3, alpha: 0.5),
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0),
                uiStatusBarStyle: UIStatusBarStyle.LightContent
            )
            return night
        case .Classic:
            var classic = ColorPalette(
                uiColorBackground: UIColor.whiteColor(),
                uiColorBackground2: UIColor(red: 239/255, green: 239/255, blue: 244/255, alpha: 1),
                uiColorTint: UIColor(red: 0, green: 122/255, blue: 1, alpha: 1),
                uiColorText: UIColor(red: 0.05, green: 0.05, blue: 0.05, alpha: 1),
                boardFontColor: SKColor(red: 0.95, green: 0.95, blue: 0.95, alpha: 1),
                boardGridColor: SKColor(red: 0.05, green: 0.05, blue: 0.06, alpha: 1),
                boardBackgroundColor: SKColor(red: 0/255, green: 148/255, blue: 61/255, alpha: 1),
                boardBlackPieceFillColor: SKColor(red: 0.05, green: 0.05, blue: 0.06, alpha: 1),
                boardBlackPieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardWhitePieceFillColor: SKColor(red: 1, green: 1, blue: 1, alpha: 1),
                boardWhitePieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardGuidePieceFillColor: SKColor(red: 0.7, green: 0.7, blue: 0.7, alpha: 1),
                boardPopupFillColor: SKColor(red: 0.3, green: 0.3, blue: 0.3, alpha: 0.5),
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0),
                uiStatusBarStyle: UIStatusBarStyle.Default
            )
            return classic
        case .ClassicDark:
            var cdark = ColorPalette(
                uiColorBackground: UIColor(red: 50/255, green: 54/255, blue: 59/255, alpha: 1),
                uiColorBackground2: UIColor(red: 55/255, green: 59/255, blue: 64/255, alpha: 1),
                uiColorTint: UIColor(red: 255/255, green: 108/255, blue: 3/255, alpha: 1),
                uiColorText: UIColor.whiteColor(),
                boardFontColor: SKColor(red: 0.95, green: 0.95, blue: 0.95, alpha: 1),
                boardGridColor: SKColor(red: 0.05, green: 0.05, blue: 0.06, alpha: 1),
                boardBackgroundColor: SKColor(red: 0/255, green: 148/255, blue: 61/255, alpha: 1),
                boardBlackPieceFillColor: SKColor(red: 0.05, green: 0.05, blue: 0.06, alpha: 1),
                boardBlackPieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardWhitePieceFillColor: SKColor(red: 1, green: 1, blue: 1, alpha: 1),
                boardWhitePieceStrokeColor: SKColor(red: 0.5, green: 0.5, blue: 0.5, alpha: 1),
                boardGuidePieceFillColor: SKColor(red: 0.7, green: 0.7, blue: 0.7, alpha: 1),
                boardPopupFillColor: SKColor(red: 0.3, green: 0.3, blue: 0.3, alpha: 0.5),
                boardPopupFontColor: SKColor(red: 0.93, green: 0.93, blue: 0.93, alpha: 1.0),
                uiStatusBarStyle: UIStatusBarStyle.LightContent
            )
            return cdark
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

    class func loadAppearanceValue() -> Appearance {
        var gc = GameSettings()
        gc.loadFromUserDefaults()

        return gc.appearance
    }

    class func applyAppearance(appearance: Appearance) {
        applyColorPalette(getColorPalette(appearance))
        UIApplication.sharedApplication().setStatusBarStyle(getColorPalette(appearance).uiStatusBarStyle, animated: true)
        persist(appearance)
    }

    class func load() -> ColorPalette {
        let appearance = loadAppearanceValue()
        applyAppearance(appearance)
        return getColorPalette(appearance)
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
