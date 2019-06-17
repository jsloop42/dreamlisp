//
//  JSVector.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSVector.h"

@implementation JSVector {
    NSMutableArray *_array;
    id<JSDataProtocol> _meta;
    NSInteger _position;
    BOOL _isImported;
    NSString *_moduleName;
}

@synthesize meta = _meta;
@synthesize value = _array;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;

+ (BOOL)isVector:(id)object {
    return [[object className] isEqual:[self className]];
}

/**
 Checks if the given data is of type `list` or `vector` and returns a `list`. Else throws an exception.

 @param data The data which needs to be converted.
 @param fnName The calling function name
 @return A list object.
 */

+ (JSList *)dataToList:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToList:data position:-1 fnName:fnName];
}

+ (JSList *)dataToList:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSList isList:data] && ![JSVector isVector:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'list' or 'vector'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'list' or 'vector'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSList *)data;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _array = [NSMutableArray new];
        [super setValue:_array];
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)list {
    self = [super init];
    if (self) {
        _array = [[NSMutableArray alloc] initWithArray:list];
        [super setValue:_array];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta vector:(JSVector *)vector {
    self = [super init];
    if (self) {
        _array = [vector value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"vector";
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:_array withBlock:block];
}

- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block {
    [_array enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:block];
}

- (JSVector *)addObject:(id<JSDataProtocol>)object {
    JSVector *vec = [self copy];
    [(NSMutableArray *)[vec value] addObject:object];
    return vec;
}

- (JSVector *)addObjectsFrom:(JSList *)list {
    JSVector *vec = [self copy];
    [[vec value] addObjectsFromArray:[list value]];
    return vec;
}

- (JSVector *)addObjectsFromHashMap:(JSHashMap *)hashMap {
    JSVector *vec = [self copy];
    NSMutableArray *arr = [vec value];
    NSArray *allKeys = [hashMap allKeys];
    id<JSDataProtocol> key = nil;
    id<JSDataProtocol> val = nil;
    for (key in allKeys) {
        val = [hashMap objectForKey:key];
        [arr addObject:[[JSVector alloc] initWithArray:[@[key, val] mutableCopy]]];
    }
    return vec;
}

- (JSList *)list {
    return [[JSList alloc] initWithArray:_array];
}

/** Returns a new list which the reverse of the current list. */
- (JSVector *)reverse {
    return [[JSVector alloc] initWithArray:[_array reverse]];
}

/** Drops n elements. */
- (JSVector *)drop:(NSInteger)n {
    return [[JSVector alloc] initWithArray:[[self value] drop:n]];
}

- (BOOL)isEqual:(id)object {
    if (![JSList isKindOfList:object]) return NO;
    JSVector *vector = (JSVector *)object;
    NSUInteger len = [_array count];
    NSUInteger i = 0;
    if (len != [vector count]) return NO;
    for (i = 0; i < len; i++) {
        if (![_array[i] isEqual:[vector nth:i]]) return NO;
    }
    return YES;
}

- (NSUInteger)hash {
    return [_array count];
}

- (JSVector *)sort:(NSInteger (*)(id, id, void *))sorter {
    return [[JSVector alloc] initWithArray:[_array sortedArrayUsingFunction:sorter context:nil]];
}

- (JSVector *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[JSVector alloc] initWithArray:[_array sortedArrayUsingComparator:comparator]];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[JSVector alloc] initWithArray:_array];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
