//
//  intrinsics.h
//  FlatReversi
//
//  Created by Kodama Yoshinori on 12/24/14.
//  Copyright (c) 2014 Yoshinori Kodama. All rights reserved.
//

#ifndef __FlatReversi__intrinsics__
#define __FlatReversi__intrinsics__

#include <stdio.h>

extern unsigned int _bitScanForward(uint64_t board);
extern unsigned int _bitPop(uint64_t board);

#endif /* defined(__FlatReversi__intrinsics__) */
