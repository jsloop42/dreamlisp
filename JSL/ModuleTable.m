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
    NSString *_name;
}

@synthesize table = _table;
@synthesize name = _name;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
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
    return [[NSString alloc] initWithFormat:@"%@\n%@", _name, [_table description]];
}

@end