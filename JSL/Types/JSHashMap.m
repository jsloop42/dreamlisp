//
//  JSHashMap.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSHashMap.h"

@implementation JSHashMap {
    NSMapTable *_table;
    id<JSDataProtocol> _meta;
    NSInteger _position;
}

@synthesize meta = _meta;

+ (BOOL)isHashMap:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data {
    return [self dataToHashMap:data position:-1];
}

+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data position:(NSInteger)position {
    if (![self isHashMap:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithArity, @"'hash-map'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatch, @"'hash-map'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSHashMap *)data;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithMapTable:(NSMapTable *)table {
    self = [super init];
    if (self) {
        _table = table;
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)array {
    self = [super init];
    if (self) {
        [self bootstrap];
        [self fromArray:array];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta hashmap:(JSHashMap *)hashmap {
    self = [super init];
    if (self) {
        _table = [hashmap value];
        _meta = meta;
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"hash-map";
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (void)fromArray:(NSArray *)array {
    NSUInteger i = 0;
    NSUInteger len = [array count];
    if (len % 2 != 0) [[[JSError alloc] initWithFormat:ArityOddError, len] throw];
    for (i = 0; i < len; i = i + 2) {
        [_table setObject:(id<JSDataProtocol>)array[i + 1] forKey:array[i]];
    }
}

- (id<JSDataProtocol>)objectForKey:(id)key {
    return [_table objectForKey:key];
}

- (void)setObject:(id<JSDataProtocol>)object forKey:(id)key {
    if ([_table containsKey:key]) [_table removeObjectForKey:key];
    [_table setObject:object forKey:key];
//    assert([_table objectForKey:key] != nil);
}

- (NSUInteger)count {
    return [_table count];
}

- (NSMapTable *)value {
    return _table;
}

- (void)setValue:(NSMapTable *)table {
    _table = table;
}

- (NSArray *)allKeys {
    return [_table allKeys];
}

- (NSArray *)allObjects {
    return [_table allObjects];
}

- (BOOL)containsKey:(id)key {
    return [_table containsKey:key];
}

- (BOOL)isEqual:(JSHashMap *)hashmap {
    if ([self count] != [hashmap count]) return NO;
    NSObject<JSDataProtocol> *lval = nil;
    id<JSDataProtocol> rval = nil;
    NSArray *keys = [self allKeys];
    NSUInteger i = 0;
    NSUInteger len = [keys count];
    for (i = 0; i < len; i++) {
        lval = [_table objectForKey:keys[i]];
        rval = [hashmap objectForKey:keys[i]];
        if (!lval || !rval || [lval isNotEqualTo:rval]) return NO;
    }
    return YES;
}

- (NSUInteger)hash {
    return [_table count];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[JSHashMap alloc] initWithMapTable:_table];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSHashMap alloc] initWithMapTable:_table];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_table description], _meta];
}

@end
