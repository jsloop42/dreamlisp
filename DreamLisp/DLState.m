//
//  DLState.m
//  DreamLisp
//
//  Created by jsloop on 05/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLState.h"

static NSUInteger _genSymCounter = 0;
static BOOL _isVerbose = NO;
static NSString *_currentModuleName;
static NSUInteger _dlObjCounter = 0;

@implementation DLState

#pragma mark Class methods

/** Increments the auto gensymn counter. */
+ (NSUInteger)counter {
    @synchronized (self) {
        return ++_genSymCounter;
    }
}

+ (NSUInteger)currentGenSymCount {
    return _genSymCounter;
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

/*! Used to generated unique count for a DLObject's associated object key */
+ (NSUInteger)assocObjectCounter {
    @synchronized (self) {
        return ++_dlObjCounter;
    }
}

+ (NSUInteger)currentAssocObjectCounter {
    return _dlObjCounter;
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
