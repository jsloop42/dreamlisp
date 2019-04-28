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
    JSData *_meta;
}

@synthesize meta = _meta;

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

- (instancetype)initWithMeta:(JSData *)meta vector:(JSVector *)vector {
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

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:_array withBlock:block];
}

- (JSList *)list {
    return [[JSList alloc] initWithArray:_array];
}

- (BOOL)isEqual:(JSVector *)vector {
    NSUInteger len = [_array count];
    NSUInteger i = 0;
    if (len != [vector count]) {
        return NO;
    }
    for (i = 0; i < len; i++) {
        if (![_array[i] isEqual:[vector nth:i]]) {
            return NO;
        }
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
    id copy = [[JSVector alloc] initWithArray:_array];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
