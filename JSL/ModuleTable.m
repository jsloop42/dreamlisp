//
//  ModuleTable.m
//  JSL
//
//  Created by jsloop on 12/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "ModuleTable.h"

/** Module table contains symbols exported by a module. */
@implementation ModuleTable {
    NSMapTable<JSSymbol *, id<JSDataProtocol>> *_table;
}

@synthesize table = _table;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (void)setObject:(id<JSDataProtocol>)obj forSymbol:(JSSymbol *)key {
    [_table setObject:obj forKey:key];
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key {
    return [_table objectForKey:key];
}

- (NSString *)description {
    return [_table description];
}

@end
