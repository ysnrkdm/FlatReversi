//
//  Zones.swift
//  FlatReversi
//
//  Created by Kodama Yoshinori on 10/26/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

import Foundation

class Zones {
    var zones: [[Double]]

    init(zones: [[Double]]) {
        self.zones = zones
    }

    init(width: Int, height: Int, initVal: Double) {
        zones = []
        for y in 0..<height {
            var row: [Double] = []
            for x in 0..<width {
                row += [initVal]
            }
            self.zones += [row]
        }
    }

    func getTopNByRandomInPuttables(n: Int, puttables: [(Int, Int)]) -> [(Int, Int)] {
        var arr: [(Double, (Int, Int))] = []
        for (x, y) in puttables {
            let ra: Double = Double(arc4random()) / Double(UINT32_MAX)
            let a = (ra * zones[y][x], (x, y))
            arr += [a]
        }

        // Sort by val
        arr.sort({
            $0.0 > $1.0
        })

        NSLog("Eval")
        for e in arr {
            NSLog("\(e.1.0), \(e.1.1) - \(e.0)")
        }

        var ret: [(Int, Int)] = []
        for elem in arr[0..<n] {
            ret += [(elem.1)]
        }

        return ret
    }

    func toString() -> String {
        var ret = ""
        for row in zones {
            for cell in row {
                ret += String(format: " %.3f ", arguments: [cell])
            }
            ret += "\n"
        }
        return ret
    }
}

class ZonesFactory {
    func createZoneUniform(uniformVal: Double) -> Zones {
        let z = Zones(width: 8, height: 8, initVal: uniformVal)
        return z
    }

    //
    // A = Corner
    // B = Neightbor of Corner
    // C = Edge 8 zones
    // D = Center 4 x 4 zones
    func createZoneTypical4(aVal: Double, bVal: Double, cVal: Double, dVal: Double) -> Zones {
        let z = Zones(width: 8, height: 8, initVal: dVal)
        for y in 0..<8 {
            for x in 0..<8 {
                if (x == 0 || x == 8) && (y == 0 || y == 7) {
                    z.zones[y][x] = aVal
                }
                if ((x == 0 || x == 7) && (y == 1 || y == 6)) || ((x == 1 || x == 6) && (y == 0 || y == 1 || y == 6 || y == 7)) {
                    z.zones[y][x] = bVal
                }
                if (x == 0 || x == 1 || x == 6 || x == 7) && (2 <= y && y <= 5) {
                    z.zones[y][x] = cVal
                }
                if (2 <= x && x <= 5) && (y == 0 || y == 1 || y == 6 || y == 7) {
                    z.zones[y][x] = cVal
                }
            }
        }
        return z
    }

    func createZoneTypical7(aVal: Double, bVal: Double, cVal: Double, dVal: Double, eVal: Double, fVal: Double, gVal: Double) -> Zones{
        var zones: [[Double]] = [
            [aVal, bVal, cVal, dVal, dVal, cVal, bVal, aVal, ],
            [bVal, cVal, eVal, eVal, eVal, eVal, cVal, bVal, ],
            [cVal, eVal, fVal, gVal, gVal, fVal, eVal, cVal, ],
            [dVal, eVal, gVal, gVal, gVal, gVal, eVal, dVal, ],
            [dVal, eVal, gVal, gVal, gVal, gVal, eVal, dVal, ],
            [cVal, eVal, fVal, gVal, gVal, fVal, eVal, cVal, ],
            [bVal, cVal, eVal, eVal, eVal, eVal, cVal, bVal, ],
            [aVal, bVal, cVal, dVal, dVal, cVal, bVal, aVal, ],
        ]

        let z = Zones(zones: zones)
        return z
    }
}