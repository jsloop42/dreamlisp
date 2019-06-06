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

- (instancetype)initWithModuleTable:(ModuleTable *)table {
    self = [super init];
    if (self) self = table;
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (void)setObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    [_table setObject:obj forKey:key];
}

- (void)updateObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    [_table updateObject:obj forKey:key];
}

- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key {
    id<JSDataProtocol> elem = [_table objectForKey:key];
    if (elem) return elem;
    if (![key hasNArity]) {
        elem = [self objectForSymbol:[key toNArity]];
        if (elem) return elem;
    }
    [key resetArity];
    return nil;
}

- (void)removeAllObjects {
    [_table removeAllObjects];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSArray<JSSymbol *> *)allKeys {
    return [_table allKeys];
}

- (NSArray<id<JSDataProtocol>> *)allObjects {
    return [_table allObjects];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"%@\n%@", _name, [_table description]];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[ModuleTable alloc] initWithModuleTable:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[ModuleTable alloc] initWithModuleTable:self];
}

@end
