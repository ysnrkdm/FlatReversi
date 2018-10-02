//
//  AppearanceManager.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import SpriteKit

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

    class func getColorPalette(_ appearance: Appearance) -> ColorPalette {
        switch appearance {
        case .LikeDarcula:
            let darcula = ColorPalette(
                uiColorBackground: UIColor(red: 52/255, green: 54/255, blue: 56/255, alpha: 1),
                uiColorBackground2: UIColor(red: 73/255, green: 76/255, blue: 78/255, alpha: 1),
                uiColorTint: UIColor(red: 216/255, green: 137/255, blue: 32/255, alpha: 1),
                uiColorText: UIColor.white,
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
                uiStatusBarStyle: UIStatusBarStyle.lightContent
            )
            return darcula
        case .WhiteGray:
            let whitegray = ColorPalette(
                uiColorBackground: UIColor.white,
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
                uiStatusBarStyle: UIStatusBarStyle.default
            )
            return whitegray
        case .Night:
            let night = ColorPalette(
                uiColorBackground: UIColor(red: 50/255, green: 54/255, blue: 59/255, alpha: 1),
                uiColorBackground2: UIColor(red: 55/255, green: 59/255, blue: 64/255, alpha: 1),
                uiColorTint: UIColor(red: 255/255, green: 108/255, blue: 3/255, alpha: 1),
                uiColorText: UIColor.white,
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
                uiStatusBarStyle: UIStatusBarStyle.lightContent
            )
            return night
        case .Classic:
            let classic = ColorPalette(
                uiColorBackground: UIColor.white,
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
                uiStatusBarStyle: UIStatusBarStyle.default
            )
            return classic
        case .ClassicDark:
            let cdark = ColorPalette(
                uiColorBackground: UIColor(red: 50/255, green: 54/255, blue: 59/255, alpha: 1),
                uiColorBackground2: UIColor(red: 55/255, green: 59/255, blue: 64/255, alpha: 1),
                uiColorTint: UIColor(red: 255/255, green: 108/255, blue: 3/255, alpha: 1),
                uiColorText: UIColor.white,
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
                uiStatusBarStyle: UIStatusBarStyle.lightContent
            )
            return cdark
        }
    }

    class func applyColorPalette(_ colorPalette: ColorPalette) {
        UINavigationBar.appearance().tintColor = colorPalette.uiColorTint
        UINavigationBar.appearance().backgroundColor = colorPalette.uiColorBackground
        UINavigationBar.appearance().barTintColor = colorPalette.uiColorBackground
        let titleDict: [String : AnyObject] = [convertFromNSAttributedStringKey(NSAttributedString.Key.foregroundColor): colorPalette.uiColorText]
        UINavigationBar.appearance().titleTextAttributes = convertToOptionalNSAttributedStringKeyDictionary(titleDict)

        UITableView.appearance().tintColor = colorPalette.uiColorTint
        UITableView.appearance().backgroundColor = colorPalette.uiColorBackground2
        UITableViewCell.appearance().tintColor = colorPalette.uiColorTint
        UITableViewCell.appearance().backgroundColor = colorPalette.uiColorBackground

        UILabel.appearance().textColor = colorPalette.uiColorText

        UIApplication.shared.setStatusBarStyle(UIStatusBarStyle.lightContent, animated: true)
        
        resetViews()
    }

    class func persist(_ appearanceToSet: Appearance) {
        let gc = GameSettings()
        gc.loadFromUserDefaults()
        gc.appearance = appearanceToSet
        gc.saveToUserDefaults()
    }

    class func loadAppearanceValue() -> Appearance {
        let gc = GameSettings()
        gc.loadFromUserDefaults()

        return gc.appearance
    }

    class func applyAppearance(_ appearance: Appearance) {
        applyColorPalette(getColorPalette(appearance))
        UIApplication.shared.setStatusBarStyle(getColorPalette(appearance).uiStatusBarStyle, animated: true)
        persist(appearance)
    }

    class func load() -> ColorPalette {
        let appearance = loadAppearanceValue()
        applyAppearance(appearance)
        return getColorPalette(appearance)
    }

    class func resetViews() {
        let windows = UIApplication.shared.windows
        for window in windows {
            let subviews = window.subviews
            for v in subviews {
                v.removeFromSuperview()
                window.addSubview(v)
            }
        }
    }
}

// Helper function inserted by Swift 4.2 migrator.
fileprivate func convertFromNSAttributedStringKey(_ input: NSAttributedString.Key) -> String {
	return input.rawValue
}

// Helper function inserted by Swift 4.2 migrator.
fileprivate func convertToOptionalNSAttributedStringKeyDictionary(_ input: [String: Any]?) -> [NSAttributedString.Key: Any]? {
	guard let input = input else { return nil }
	return Dictionary(uniqueKeysWithValues: input.map { key, value in (NSAttributedString.Key(rawValue: key), value)})
}
