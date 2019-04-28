//
//  JSVector.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSVector.h"

@implementation JSVector {
    NSMutableArray *array;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        array = [NSMutableArray new];
        [super setValue:array];
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)list {
    self = [super init];
    if (self) {
        array = [[NSMutableArray alloc] initWithArray:list];
        [super setValue:array];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta vector:(JSVector *)vector {
    self = [super init];
    if (self) {
        array = [vector value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:array withBlock:block];
}

- (JSList *)list {
    return [[JSList alloc] initWithArray:array];
}

- (BOOL)isEqual:(JSVector *)vector {
    NSUInteger len = [array count];
    NSUInteger i = 0;
    if (len != [vector count]) {
        return NO;
    }
    for (i = 0; i < len; i++) {
        if (![array[i] isEqual:[vector nth:i]]) {
            return NO;
        }
    }
    return YES;
}

- (NSUInteger)hash {
    return [array count];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSVector alloc] initWithArray:array];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [array description], _meta];
}

@end
