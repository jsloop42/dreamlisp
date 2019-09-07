//
//  DLVector.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLVector.h"

@implementation DLVector {
    NSMutableArray *_array;
    id<DLDataProtocol> _meta;
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

+ (DLList *)dataToList:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToList:data position:-1 fnName:fnName];
}

+ (DLList *)dataToList:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLList isList:data] && ![DLVector isVector:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'list' or 'vector'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'list' or 'vector'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLList *)data;
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

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta vector:(DLVector *)vector {
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

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:_array withBlock:block];
}

- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block {
    [_array enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:block];
}

- (DLVector *)addObject:(id<DLDataProtocol>)object {
    @autoreleasepool {
        DLVector *vec = [self copy];
        [(NSMutableArray *)[vec value] addObject:object];
        return vec;
    }
}

- (DLList *)list {
    return [[DLList alloc] initWithArray:_array];
}

/** Returns a new list which the reverse of the current list. */
- (DLVector *)reverse {
    return [[DLVector alloc] initWithArray:[_array reverse]];
}

/** Drops n elements. */
- (DLVector *)drop:(NSInteger)n {
    return [[DLVector alloc] initWithArray:[[self value] drop:n]];
}

- (BOOL)isEqual:(id)object {
    if (![DLList isKindOfList:object]) return NO;
    DLVector *vector = (DLVector *)object;
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

- (DLVector *)sort:(NSInteger (*)(id, id, void *))sorter {
    return [[DLVector alloc] initWithArray:[_array sortedArrayUsingFunction:sorter context:nil]];
}

- (DLVector *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[DLVector alloc] initWithArray:[_array sortedArrayUsingComparator:comparator]];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLVector alloc] initWithArray:_array];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    NSMutableString *str = [NSMutableString new];
    NSUInteger len = [_array count];
    NSUInteger last = len - 1;
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        [str appendString:[_array[i] description]];
        if (i != last) [str appendString:@" "];
    }
    return [NSString stringWithFormat:@"[%@]", str];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
