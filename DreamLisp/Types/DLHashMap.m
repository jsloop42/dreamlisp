//
//  DLHashMap.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLHashMap.h"

@implementation DLHashMap {
    NSMapTable *_table;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _table;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isHashMap:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLHashMap *)dataToHashMap:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToHashMap:data position:-1 fnName:fnName];
}

+ (DLHashMap *)dataToHashMap:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLHashMap isHashMap:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'hash-map'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'hash-map'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLHashMap *)data;
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

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta hashmap:(DLHashMap *)hashmap {
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

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (void)fromArray:(NSArray *)array {
    NSUInteger i = 0;
    NSUInteger len = [array count];
    if (len % 2 != 0) [[[DLError alloc] initWithFormat:ArityOddError, len] throw];
    for (i = 0; i < len; i = i + 2) {
        [_table setObject:(id<DLDataProtocol>)array[i + 1] forKey:array[i]];
    }
}

- (id<DLDataProtocol>)objectForKey:(id)key {
    return [_table objectForKey:key];
}

- (void)setObject:(id<DLDataProtocol>)object forKey:(id)key {
    if ([_table containsKey:key]) [_table removeObjectForKey:key];
    [_table setObject:object forKey:key];
//    assert([_table objectForKey:key] != nil);
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
    if (![DLHashMap isHashMap:object]) return NO;
    DLHashMap *hashmap = (DLHashMap *)object;
    if ([self count] != [hashmap count]) return NO;
    NSObject<DLDataProtocol> *lval = nil;
    id<DLDataProtocol> rval = nil;
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
    id elem = [[DLHashMap alloc] initWithMapTable:_table];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    NSArray *allKeys = [self allKeys];
    NSMutableString *str = [NSMutableString new];
    id<DLDataProtocol> key = nil;
    for (key in allKeys) {
        [str appendFormat:@"%@ %@", [key description], [[self objectForKey:key] description]];
    }
    return [NSString stringWithFormat:@"{%@}", str];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_table description], _meta];
}

@end
