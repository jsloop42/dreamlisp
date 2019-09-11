//
//  DLModuleTable.m
//  DreamLisp
//
//  Created by jsloop on 12/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLModuleTable.h"

/** Module table contains symbols exported by a module. */
@implementation DLModuleTable {
    NSMapTable<DLSymbol *, id<DLDataProtocol>> *_table;
    NSString *_name;
}

@synthesize table = _table;
@synthesize name = _name;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (instancetype)initWithModuleTable:(DLModuleTable *)table {
    self = [super init];
    if (self) self = table;
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _table = [coder decodeObjectOfClass:[self classForCoder] forKey:@"ModuleTable_table"];
        _name = [coder decodeObjectOfClass:[self classForCoder] forKey:@"ModuleTable_name"];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_table forKey:@"ModuleTable_table"];
    [coder encodeObject:_name forKey:@"ModuleTable_name"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (void)setObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key {
    [_table setObject:obj forKey:key];
}

- (void)updateObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key {
    [_table updateObject:obj forKey:key];
}

- (id<DLDataProtocol> _Nullable)objectForSymbol:(DLSymbol *)key {
    id<DLDataProtocol> elem = [_table objectForKey:key];
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

- (NSArray<DLSymbol *> *)allKeys {
    return [_table allKeys];
}

- (NSArray<id<DLDataProtocol>> *)allObjects {
    return [_table allObjects];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"%@\n%@", _name, [_table description]];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[DLModuleTable alloc] initWithModuleTable:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[DLModuleTable alloc] initWithModuleTable:self];
}

@end
