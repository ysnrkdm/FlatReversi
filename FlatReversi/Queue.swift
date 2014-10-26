//
//  Queue.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class QNode<T> {
    var key: T? = nil
    var next: QNode? = nil
}

class Queue<T> {
    private var top: QNode<T>! = QNode<T>()

    func enqueue(var key: T) {
        if top == nil {
            top = QNode()
        }
        if top.key == nil {
            top.key = key; return
        }

        var childToUse: QNode<T> = QNode<T>()
        var current: QNode = top

        while current.next != nil {
            current = current.next!
        }

        childToUse.key = key
        current.next = childToUse
    }

    func dequeue() -> T? {
        let topitem = self.top?.key
        if topitem == nil {
            return nil
        }

        var queueitem = top.key!

        if let nextitem = top.next {
            top = nextitem
        } else {
            top = nil
        }

        return queueitem
    }

    func isEmpty() -> Bool {
        if let topitem = self.top?.key {
            return false
        } else {
            return true
        }
    }

    func peek() -> T? {
        return top?.key
    }
}