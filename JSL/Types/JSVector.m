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
}

@synthesize meta = _meta;
@synthesize value = _array;

+ (BOOL)isVector:(id)object {
    return [[object className] isEqual:[self className]];
}

/**
  Checks if the given data is of type `list` or `vector` and returns a `list`. Else throws an exception.

  @param data The data which needs to be converted.
  @return A list object.
 */
+ (JSList *)dataToList:(id<JSDataProtocol>)data {
    return [self dataToList:data position:-1];
}

+ (JSList *)dataToList:(id<JSDataProtocol>)data position:(NSInteger)position {
    if (![JSList isList:data] && ![JSVector isVector:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithArity, @"'list' or 'vector'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatch, @"'list' or 'vector'", [data dataTypeName]];
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

- (JSList *)list {
    return [[JSList alloc] initWithArray:_array];
}

- (BOOL)isEqual:(JSVector *)vector {
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

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[JSVector alloc] initWithArray:_array];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSVector alloc] initWithArray:_array];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
