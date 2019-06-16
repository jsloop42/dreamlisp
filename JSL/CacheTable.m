//
//  CacheTable.m
//  JSL
//
//  Created by jsloop on 16/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "CacheTable.h"

/** A cache table which uses MRU (Most Recently Used) algorithm for caching. */
@implementation CacheTable {
    NSMapTable *_table;
    NSMutableArray *_mru;
    NSUInteger _mruSize;
}

@synthesize table = _table;
@synthesize mru = _mru;
@synthesize mruSize = _mruSize;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithSize:(NSUInteger)size {
    self = [super init];
    if (self) {
        [self bootstrap];
        _mruSize = size;
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    _mru = [NSMutableArray new];
    _mruSize = 64;
}

- (id _Nullable)objectForKey:(id)key {
    return [_table objectForKey:key];
}

- (void)setObject:(id)object forKey:(id)key {
    if ([_table count] == _mruSize) {
        [_table removeObjectForKey:[_mru drop]];
    }
    [_table setObject:object forKey:key];
    [_mru addObject:key];
}

- (void)clear {
    [_table removeAllObjects];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSArray *)allKeys {
    return [_table allKeys];
}

- (NSArray *)allObjects {
    return [_table allObjects];
}

@end
