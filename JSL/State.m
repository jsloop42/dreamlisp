//
//  State.m
//  JSL
//
//  Created by jsloop on 05/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "State.h"

static NSUInteger _genSymCounter = 0;
static BOOL _isVerbose = NO;

@implementation State

/** Increments the auto gensymn counter. */
+ (NSUInteger)counter {
    return ++_genSymCounter;
}

+ (void)setIsVerbose:(BOOL)flag {
    _isVerbose = flag;
}

+ (BOOL)isVerbose {
    return _isVerbose;
}

/** Increments the auto gensymn counter. */
- (NSUInteger)counter {
    return ++_genSymCounter;
}

/** Return the current auto gensymn counter. */
- (NSInteger)currentCounter {
    return _genSymCounter;
}

@end
