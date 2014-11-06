//
//  Stack.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/30/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class Stack<T> {

    var elements = [T]()

    func push(elem: T) {
        elements.append(elem)
    }

    func pop() -> T? {
        if elements.isEmpty {
            return nil
        }
        return elements.removeLast()
    }

    func peek() -> T? {
        if elements.isEmpty {
            return nil
        }
        return elements[elements.endIndex-1]
    }

    var count: Int {
        return elements.count
    }

    var isEmpty: Bool {
        return elements.isEmpty
    }
}