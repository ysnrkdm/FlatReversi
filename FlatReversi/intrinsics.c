//
//  intrinsics.c
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/24/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

#include "intrinsics.h"

unsigned int _bitScanForward(uint64_t board) {
    int ret = __builtin_ctzll(board);
    return ret;
}

unsigned int _bitPop(uint64_t board) {
    int ret = __builtin_popcountll(board);
    return ret;
}