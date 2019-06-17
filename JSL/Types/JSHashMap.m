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
    BOOL _isImported;
    NSString *_moduleName;
}

@synthesize meta = _meta;
@synthesize value = _table;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;

+ (BOOL)isHashMap:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToHashMap:data position:-1 fnName:fnName];
}

+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSHashMap isHashMap:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'hash-map'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'hash-map'", [data dataTypeName]];
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

- (JSHashMap *)addObjectsFrom:(JSList *)list {
    JSHashMap *hm = [self copy];
    [hm fromArray:[list value]];
    return hm;
}

- (JSHashMap *)addObjectsFromHashMap:(JSHashMap *)hashMap {
    JSHashMap *hm = [self copy];
    NSArray *allKeys = [hashMap allKeys];
    id<JSDataProtocol> key = nil;
    for (key in allKeys) {
        [hm setObject:[hashMap objectForKey:key] forKey:key];
    }
    return hm;
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

- (BOOL)containsKey:(id)key {
    return [_table containsKey:key];
}

- (BOOL)isEqual:(id)object {
    if (![JSHashMap isHashMap:object]) return NO;
    JSHashMap *hashmap = (JSHashMap *)object;
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

- (NSInteger)sortValue {
    return [self hash];
}

/** Returns an array containing sorted keys. */
- (NSArray *)sortKeys:(NSInteger (*)(id, id, void *))sorter {
    return [[_table allKeys] sortedArrayUsingFunction:sorter context:nil];
}

/** Returns an array containing sorted objects. */
- (NSArray *)sortObjects:(NSInteger (*)(id, id, void *))sorter {
    return [[_table allObjects] sortedArrayUsingFunction:sorter context:nil];
}

- (NSArray *)sortedKeysUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[_table allKeys] sortedArrayUsingComparator:comparator];
}

- (NSArray *)sortedObjectsUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[_table allObjects] sortedArrayUsingComparator:comparator];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[JSHashMap alloc] initWithMapTable:_table];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_table description], _meta];
}

@end
