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
static BOOL _isConcurrentMode = YES;

@implementation State

#pragma mark Class methods

/** Increments the auto gensymn counter. */
+ (NSUInteger)counter {
    @synchronized (self) {
        return ++_genSymCounter;
    }
}

+ (void)setIsVerbose:(BOOL)flag {
    _isVerbose = flag;
}

+ (BOOL)isVerbose {
    return _isVerbose;
}

+ (BOOL)isConcurrentMode {
    return _isConcurrentMode;
}

+ (void)setIsConcurrentMode {

}

#pragma mark Instance methods

/** Increments the auto gensymn counter. */
- (NSUInteger)counter {
    @synchronized (self) {
        return ++_genSymCounter;
    }
}

/** Return the current auto gensymn counter. */
- (NSInteger)currentCounter {
    return _genSymCounter;
}

@end
