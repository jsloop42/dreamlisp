//
//  DLState.m
//  DreamLisp
//
//  Created by Jaseem V V on 05/05/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLState.h"

static NSUInteger _genSymCounter = 0;
static BOOL _isVerbose = NO;
static NSString *_currentModuleName;

@implementation DLState

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

+ (NSString *)currentModuleName {
    return _currentModuleName;
}

+ (void)setCurrentModuleName:(NSString *)name {
    @synchronized (self) {
        _currentModuleName = name;
    }
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
