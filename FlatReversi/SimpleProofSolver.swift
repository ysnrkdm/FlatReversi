//
//  SimpleProofSolver.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/29/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

enum NodeType {
    case and, or
}

enum ProofType {
    case proven, disproven, unknown
}

class ProofNode {
    var value: ProofType = .unknown
    var proof: Int = 1
    var disproof : Int = 1

    var expanded : Bool = false

    var whosTurn: Pieces = .none

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
            ret += 1
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
        let stack = Stack<ProofNode>()
        stack.push(self)
        while !stack.isEmpty {
            if let current = stack.pop() {
                // Add children to stack
                for child in current.children.sorted(by: {$0.proof < $1.proof}) {
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
//            ret = spaces + "\(whosTurn.toString()) \(proof) : \(pf) -- put \(put.0), \(put.1)\n\(boardRepresentation.toString())\n"
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

    override func solve(_ boardRepresentation: BoardRepresentation, forPlayer: Pieces) -> ProofAnswer {
        let root = ProofNode(value: .unknown, proof: 1, disproof: 1, whosTurn: forPlayer, boardRepresentation: boardRepresentation, parent: nil)

        pns(root)

        if root.proof == 0 {
            var moves: [(Int, Int)] = []
            for c in root.children {
                if c.proof == 0 {
                    let puts = c.put
                    moves.append(puts)
                }
            }

            switch forPlayer {
            case .black:
                return ProofAnswer(proof: .blackWin, moves: moves)
            case .white:
                return ProofAnswer(proof: .whiteWin, moves: moves)
            default:
                return ProofAnswer(proof: .undefined, moves: [])
            }
        } else {
            return ProofAnswer(proof: .undefined, moves: [])
        }
    }

    func pns(_ root: ProofNode) {
        evaluate(root, attacker: root.whosTurn)
        setProofAndDisproofNumbers(root, attacker: root.whosTurn)

        var current = root

        while root.proof != 0 && root.disproof != 0 && resourcesAvailable() {
            let mostProving = selectMostProvingNode(current, attacker: root.whosTurn)
            expandNode(mostProving, attacker: root.whosTurn)
            current = updateAncestors(mostProving, root: root)
        }
    }

    func evaluate(_ node: ProofNode, attacker: Pieces) {
        node.value = .unknown
        let a = isTerminal(node.boardRepresentation)
        if a == .blackWin {
            if attacker == .black {
                node.value = .proven
            } else {
                node.value = .disproven
            }
        } else if a == .whiteWin {
            if attacker == .white {
                node.value = .proven
            } else {
                node.value = .disproven
            }
        }
    }

    func setProofAndDisproofNumbers(_ node: ProofNode, attacker: Pieces) {
        if ( node.expanded ) {
            // Interior node
            if ( nodeType(node, attacker: attacker) == .and ) {
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
            case .disproven:
                node.proof = INF; node.disproof = 0
            case .proven:
                node.proof = 0; node.disproof = INF
            case .unknown:
                node.proof = 1; node.disproof = 1
            }
        }
    }

    func nodeType(_ node: ProofNode, attacker: Pieces) -> NodeType {
        if node.whosTurn == attacker {
            return .or
        } else {
            return .and
        }
    }

    func resourcesAvailable() -> Bool {
        return true
    }

    func selectMostProvingNode(_ node: ProofNode, attacker: Pieces) -> ProofNode {
        var ret = node
        while ( ret.expanded ) {
            var value = INF;
            var best = ProofNode(value: .unknown, proof: 1, disproof: 1, whosTurn: .black, boardRepresentation: BoardRepresentation(boardMediator: BoardMediator(board: SimpleBitBoard())), parent: nil, children: []);
            if ( nodeType(ret, attacker: attacker) == .and ) {
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

    func expandNode(_ node: ProofNode, attacker: Pieces) {
        // Generate children
        let children = getChildren(node)
        node.children = children

        for c in node.children {
            evaluate(c, attacker: attacker)
            setProofAndDisproofNumbers(c, attacker: attacker)
            if nodeType(node, attacker: attacker) == NodeType.and {
                // And node
                if c.disproof == 0 { break }
            } else {
                // OR node
                if c.proof == 0 { break }
            }
        }
        node.expanded = true
    }

    func updateAncestors(_ from: ProofNode, root: ProofNode) -> ProofNode {
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

    func getChildren(_ node: ProofNode) -> [ProofNode] {
        var ret: [ProofNode] = []

        for (px, py) in node.boardRepresentation.getPuttables(node.whosTurn) {
            let newBR = node.boardRepresentation.clone()
            newBR.boardMediator.put(node.whosTurn, x: px, y: py)
            let newNode = ProofNode(value: .unknown, proof: 1, disproof: 1, whosTurn: node.boardRepresentation.boardMediator.nextTurn(node.whosTurn), boardRepresentation: newBR, parent: node, children: [])
            newNode.put = (px, py)
            ret.append(newNode)
        }

        return ret
    }

    func isTerminal(_ boardRepresentation: BoardRepresentation) -> Proofs {
        if boardRepresentation.getNumVacant() > 0 || boardRepresentation.getPuttables(Pieces.black).count > 0 || boardRepresentation.getPuttables(Pieces.white).count > 0 {
            return .undefined
        } else {
            if boardRepresentation.getNumBlack() > boardRepresentation.getNumWhite() {
                return .blackWin
            } else if boardRepresentation.getNumBlack() < boardRepresentation.getNumWhite() {
                return .whiteWin
            } else {
                return .draw
            }
        }
    }
}
