//
//  FlatReversiTests.swift
//  FlatReversiTests
//
//  Created by Kodama Yoshinori on 10/24/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import UIKit
import XCTest
import Graphene
import Firebase

@testable import FlatReversi

class FlatReversiTests: XCTestCase {
    
    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    func testExample() {
        // This is an example of a functional test case.
        XCTAssert(true, "Pass")

        let b = BoardBuilder.build(
            "........" +
            "........" +
            "........" +
            "...BW..." +
            "...WB..." +
            "........" +
            "........" +
            "........"
        )
        NSLog(b.toString())
    }
    
//    func testPerformanceExample() {
//        // This is an example of a performance test case.
//        self.measureBlock() {
//            // Put the code you want to measure the time of here.
//        }
//    }

}
