//
//  SimpleProofSolver.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/29/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum NodeType {
    case And, Or
}

enum ProofType {
    case Proven, Disproven, Unknown
}

class ProofNode {
    var value: ProofType = .Unknown
    var proof: Int = 1
    var disproof : Int = 1

    var expanded : Bool = false

    var whosTurn: Pieces = .None

    var parent: ProofNode?
    var children: [ProofNode] = []

    // MARK: Payloads
    var boardRepresentation: BoardRepresentation
    var put: (Int, Int) = (-1, -1)

    var depth : Int {
        var ret = 0
        var current = self
        while current.parent != nil {
            current = current.parent!
            ++ret
        }
        return ret
    }

    init(value: ProofType, proof: Int, disproof : Int, whosTurn: Pieces, boardRepresentation: BoardRepresentation, parent: ProofNode?, children: [ProofNode] = []) {
        self.value = value
        self.proof = proof
        self.disproof = disproof

        self.whosTurn = whosTurn

        self.parent = parent
        self.children = children

        self.boardRepresentation = boardRepresentation.clone()
    }

    func toString() -> String {
        var ret = ""
        var stack = Stack<ProofNode>()
        stack.push(self)
        while !stack.isEmpty {
            if let current = stack.pop() {
                // Add children to stack
                var children = current.children
                // Sort by proofnum
                children.sort({$0.proof < $1.proof})

                for child in children {
                    stack.push(child)
                }

                // Do the job
                ret += current.nodeToString()
            }
        }

        return ret
    }

    func nodeToString() -> String {
        var spaces = ""
        for i in 0..<depth {
            if i == 0 && depth == 1 {
                spaces += "+"
            } else if i == depth - 1 {
                spaces += "+"
            } else {
                spaces += " "
            }
        }
        let pf = proof == Int.max ? "Inf" : String(proof)
        var ret = ""
        let showBoardRep = false
        if showBoardRep {
            ret = spaces + "\(whosTurn.toString()) \(proof) : \(pf) -- put \(put.0), \(put.1)\n\(boardRepresentation.toString())\n"
        } else {
            ret = spaces + "\(whosTurn.toString()) \(proof) : \(pf) -- put \(put.0), \(put.1)\n"
        }

        return ret
    }
}

class BoardCache {
    var cache: Dictionary<String, Proofs> = Dictionary<String, Proofs>()
}

class SimpleProofSolver: ProofSolver {
    let INF = Int.max / 10

    override func solve(boardRepresentation: BoardRepresentation, forPlayer: Pieces) -> ProofAnswer {
        let root = ProofNode(value: .Unknown, proof: 1, disproof: 1, whosTurn: forPlayer, boardRepresentation: boardRepresentation, parent: nil)

        pns(root)

//        println("Answer : \n" + root.toString())


        if root.proof == 0 {
            var moves: [(Int, Int)] = []
            for c in root.children {
                if c.proof == 0 {
                    let puts = c.put
                    moves.append(puts)
                }
            }

            switch forPlayer {
            case .Black:
                return ProofAnswer(proof: .BlackWin, moves: moves)
            case .White:
                return ProofAnswer(proof: .WhiteWin, moves: moves)
            default:
                return ProofAnswer(proof: .Undefined, moves: [])
            }
        } else {
            return ProofAnswer(proof: .Undefined, moves: [])
        }
    }

    func pns(root: ProofNode) {
        evaluate(root, attacker: root.whosTurn)
        setProofAndDisproofNumbers(root, attacker: root.whosTurn)

        var current = root

        while root.proof != 0 && root.disproof != 0 && resourcesAvailable() {
//            println("in loop")
            let mostProving = selectMostProvingNode(current, attacker: root.whosTurn)
//            println("most proving : " + mostProving.toString())
            expandNode(mostProving, attacker: root.whosTurn)
//            println("expanded. update ancestors")
            current = updateAncestors(mostProving, root: root)
//            println("done. next loop...")
//            println(current.toString())
        }
    }

    func evaluate(node: ProofNode, attacker: Pieces) {
        node.value = .Unknown
        let a = isTerminal(node.boardRepresentation)
        if a == .BlackWin {
            if attacker == .Black {
                node.value = .Proven
            } else {
                node.value = .Disproven
            }
        } else if a == .WhiteWin {
            if attacker == .White {
                node.value = .Proven
            } else {
                node.value = .Disproven
            }
        }
    }

    func setProofAndDisproofNumbers(node: ProofNode, attacker: Pieces) {
        if ( node.expanded ) {
            // Interior node
            if ( nodeType(node, attacker: attacker) == .And ) {
                // AND node
                node.proof = 0
                node.disproof = INF
                for c in node.children {
                    node.proof += c.proof;
                    node.disproof = min([node.disproof, c.disproof]);
                }
            } else {
                // OR node
                node.proof = INF;  node.disproof = 0;
                for c in node.children {
                    node.disproof += c.disproof;
                    node.proof = min([node.proof, c.proof]);
                }
            }
        } else {
            // Terminal node or none terminal leaf
            switch( node.value ) {
            case .Disproven:
                node.proof = INF; node.disproof = 0
            case .Proven:
                node.proof = 0; node.disproof = INF
            case .Unknown:
                node.proof = 1; node.disproof = 1
            }
        }
    }

    func nodeType(node: ProofNode, attacker: Pieces) -> NodeType {
        if node.whosTurn == attacker {
            return .Or
        } else {
            return .And
        }
    }

    func resourcesAvailable() -> Bool {
        return true
    }

    func selectMostProvingNode(node: ProofNode, attacker: Pieces) -> ProofNode {
        var ret = node
        while ( ret.expanded ) {
            var value = INF;
            var best = ProofNode(value: .Unknown, proof: 1, disproof: 1, whosTurn: .Black, boardRepresentation: BoardRepresentation(boardMediator: BoardMediator()), parent: nil, children: []);
            if ( nodeType(ret, attacker: attacker) == .And ) {
                for c in ret.children {
                    if ( value > c.disproof ) {
                        best = c;
                        value = c.disproof;
                    }
                }
            } else { /* OR node */
                for c in ret.children {
                    if ( value > c.proof ) {
                        best = c;
                        value = c.proof;
                    }
                }
            }
            ret = best;
        }
        return ret;
    }

    func expandNode(node: ProofNode, attacker: Pieces) {
        // Generate children
        let children = getChildren(node)
        node.children = children

        for c in node.children {
            evaluate(c, attacker: attacker)
            setProofAndDisproofNumbers(c, attacker: attacker)
            if nodeType(node, attacker: attacker) == NodeType.And {
                // And node
                if c.disproof == 0 { break }
            } else {
                // OR node
                if c.proof == 0 { break }
            }
        }
        node.expanded = true
    }

    func updateAncestors(from: ProofNode, root: ProofNode) -> ProofNode {
        var node: ProofNode = from

        while node !== root {
            let oldProof = node.proof
            let oldDisproof = node.disproof
            setProofAndDisproofNumbers(node, attacker: root.whosTurn)
            if node.proof == oldProof && node.disproof == oldDisproof {
                return node
            }
            node = node.parent!
        }

        setProofAndDisproofNumbers(root, attacker: root.whosTurn)

        return root
    }

    func getChildren(node: ProofNode) -> [ProofNode] {
        var ret: [ProofNode] = []

        for (px, py) in node.boardRepresentation.getPuttables(node.whosTurn) {
            let newBR = node.boardRepresentation.clone()
            newBR.boardMediator.put(node.whosTurn, x: px, y: py)
            let newNode = ProofNode(value: .Unknown, proof: 1, disproof: 1, whosTurn: node.boardRepresentation.boardMediator.nextTurn(node.whosTurn), boardRepresentation: newBR, parent: node, children: [])
            newNode.put = (px, py)
            ret.append(newNode)
        }

        return ret
    }

    func isTerminal(boardRepresentation: BoardRepresentation) -> Proofs {
        if boardRepresentation.getNumVacant() > 0 || boardRepresentation.getPuttables(Pieces.Black).count > 0 || boardRepresentation.getPuttables(Pieces.White).count > 0 {
            return .Undefined
        } else {
            if boardRepresentation.getNumBlack() > boardRepresentation.getNumWhite() {
//                println("Black Win")
//                println(boardRepresentation.toString())
                return .BlackWin
            } else if boardRepresentation.getNumBlack() < boardRepresentation.getNumWhite() {
//                println("White Win")
//                println(boardRepresentation.toString())
                return .WhiteWin
            } else {
                return .Draw
            }
        }
    }

    func sum (var array : [Int]) -> Int {
        if array.isEmpty {
            return 0
        }
        return reduce(array, array[0]) {$0 + $1}
    }

    func min (var array : [Int]) -> Int {
        if array.isEmpty {
            return Int.max
        }
        return reduce(array, array[0]) {$0 > $1 ? $1 : $0}
    }
    
    func max (var array : [Int]) -> Int {
        if array.isEmpty {
            return Int.min
        }
        return reduce(array, array[0]) {$0 < $1 ? $1 : $0}
    }
}