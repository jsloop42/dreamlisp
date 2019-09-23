//
//  DLState.m
//  DreamLisp
//
//  Created by jsloop on 05/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLState.h"

static DLState *_state;

@implementation DLState {
    NSUInteger _genSymCounter;
    NSUInteger _dlObjCounter;
}

@synthesize currentGenSymCount = _genSymCounter;
@synthesize currentAssocObjectCount = _dlObjCounter;

+ (instancetype)shared {
    @synchronized (self) {
        if (!_state) {
            _state = [DLState new];
        }
        return _state;
    }
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _prefixTree = [DLTrie new];
        _userPrefixTree = [DLTrie new];
        _genSymCounter = 0;
        _isVerbose = NO;
        _dlObjCounter = 0;
    }
    return self;
}

/*!
 Initializes the internal trie with the prefixes from the given array.
 */
- (void)initPrefixes:(NSArray *)prefixes {
    NSEnumerator *iter = [prefixes objectEnumerator];
    NSString *str = nil;
    while ((str = [iter nextObject]) !=  nil) {
        [_prefixTree insert:str];
    }
}

#pragma mark - Gensym Counter

- (NSUInteger)genSymCounter {
    return ++_genSymCounter;
}

#pragma mark - Assoc Object Counter

- (NSUInteger)assocObjectCounter {
    return ++_dlObjCounter;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _prefixTree = [coder decodeObjectOfClass:[DLTrie classForCoder] forKey:@"DLState_prefixTree"];
        _userPrefixTree = [coder decodeObjectOfClass:[DLTrie classForCoder] forKey:@"DLState_userPrefixTree"];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_prefixTree forKey:@"DLState_prefixTree"];
    [coder encodeObject:_userPrefixTree forKey:@"DLState_userPrefixTree"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)addPrefix:(NSString *)prefix {
    [_userPrefixTree insert:prefix];
}

- (void)removePrefix:(NSString *)prefix {
    [_userPrefixTree delete:prefix];
}

@end
