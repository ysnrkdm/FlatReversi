//
//  Appearance.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 1/11/15.
//  Copyright (c) 2015 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum Appearance: String {
    case WhiteGray = "WhiteGray", LikeDarcula = "LikeDarcula", Night = "Night", Classic = "Classic", ClassicDark = "ClassicDark"

    static func toList() -> [Appearance] {
        return [.WhiteGray, .LikeDarcula, .Night, .Classic, .ClassicDark]
    }
}