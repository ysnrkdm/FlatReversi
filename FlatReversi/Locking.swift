//
//  Locking.swift
//  MyFirstSpriteKit
//
//  Created by kristopherjohnson / locking.swift
//  https://gist.github.com/kristopherjohnson/d12877ee9a901867f599
//

import Foundation

// MARK: Multi-thread utility functions
func dispatch_async_main(_ block: @escaping () -> ()) {
    DispatchQueue.main.async(execute: block)
}

func dispatch_async_global(_ block: @escaping () -> ()) {
    DispatchQueue.global(priority: DispatchQueue.GlobalQueuePriority.default).async(execute: block)
}

/// Protocol for NSLocking objects that also provide tryLock()
public protocol TryLockable: NSLocking {
    func `try`() -> Bool
}

// These Cocoa classes have tryLock()
extension NSLock: TryLockable {}
extension NSRecursiveLock: TryLockable {}
extension NSConditionLock: TryLockable {}


/// Protocol for NSLocking objects that also provide lockBeforeDate()
public protocol BeforeDateLockable: NSLocking {
    func lock(before limit: Date) -> Bool
}

// These Cocoa classes have lockBeforeDate()
extension NSLock: BeforeDateLockable {}
extension NSRecursiveLock: BeforeDateLockable {}
extension NSConditionLock: BeforeDateLockable {}


/// Use an NSLocking object as a mutex for a critical section of code
public func synchronized<L: NSLocking>(_ lockable: L, criticalSection: () -> ()) {
    lockable.lock()
    criticalSection()
    lockable.unlock()
}

/// Use an NSLocking object as a mutex for a critical section of code that returns a result
public func synchronizedResult<L: NSLocking, T>(_ lockable: L, criticalSection: () -> T) -> T {
    lockable.lock()
    let result = criticalSection()
    lockable.unlock()
    return result
}

/// Use a TryLockable object as a mutex for a critical section of code
///
/// Return true if the critical section was executed, or false if tryLock() failed
public func trySynchronized<L: TryLockable>(_ lockable: L, criticalSection: () -> ()) -> Bool {
    if !lockable.try() {
        return false
    }
    criticalSection()
    lockable.unlock()
    return true
}

/// Use a BeforeDateLockable object as a mutex for a critical section of code
///
/// Return true if the critical section was executed, or false if lockBeforeDate() failed
public func synchronizedBeforeDate<L: BeforeDateLockable>(_ limit: Date, lockable: L, criticalSection: () -> ()) -> Bool {
    if !lockable.lock(before: limit) {
        return false
    }
    criticalSection()
    lockable.unlock()
    return true
}


//// Examples
//
//let lock = NSLock()
//
//synchronized(lock) {
//    println("This is synchronized")
//}
//
//let result: String = synchronizedResult(lock) {
//    return "This is synchronized"
//}
//
//let didRun = trySynchronized(lock) {
//    println("This was synchronized with a trySynchronized")
//}
//println("didRun: \(didRun)")
//
//let limit = NSDate(timeIntervalSinceNow: 1.0)
//let didRunBeforeDate = synchronizedBeforeDate(limit, lock) {
//    println("This was synchronized with a time limit");
//}
//println("didRunBeforeDate: \(didRunBeforeDate)")
//
//
//let recursiveLock = NSRecursiveLock()
//
//synchronized(recursiveLock) {
//    println("This is synchronized with a recursive lock")
//    synchronized(recursiveLock) {
//        println("This is synchronized with the same recursive lock")
//    }
//}
