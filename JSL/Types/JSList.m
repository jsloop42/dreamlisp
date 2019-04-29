//
//  JSList.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSList.h"

@implementation JSList {
    NSMutableArray *_array;
    JSData *_meta;
}

@synthesize meta = _meta;

+ (BOOL)isList:(id)object {
    if ([object isKindOfClass:[self class]]) {
        return YES;
    }
    return NO;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _array = [NSMutableArray new];
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)list {
    self = [super init];
    if (self) {
        _array = [[NSMutableArray alloc] initWithArray:list];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta list:(JSList *)list {
    self = [super init];
    if (self) {
        _array = [list value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"list";
}

- (void)add:(JSData *)object {
    [_array addObject:object];
}

- (void)add:(JSData *)object atIndex:(NSUInteger)index {
    [_array insertObject:object atIndex:index];
}

- (void)remove:(JSData *)object {
    [_array removeObject:object];
}

- (void)removeAtIndex:(NSUInteger)index {
    [_array removeObjectAtIndex:index];
}

- (void)setValue:(NSMutableArray *)aArray {
    _array = aArray;
}

- (NSMutableArray *)value {
    return _array;
}

- (NSUInteger)count {
    return [_array count];
}

- (JSData *)first {
    return [_array firstObject];
}

- (JSData *)second {
    return [_array objectAtIndex:1];
}

- (JSData *)rest {
    NSMutableArray *arr = [_array mutableCopy];
    [arr removeObjectAtIndex:0];
    return [[JSList alloc] initWithArray:arr];
}

- (JSData *)last {
    return [_array objectAtIndex:[_array count] - 1];
}

- (JSData *)dropLast {
    NSMutableArray *arr = [_array mutableCopy];
    [arr removeLastObject];
    return [[JSList alloc] initWithArray:arr];
}

- (JSData *)nth:(NSInteger)n {
    return [_array objectAtIndex:n];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:_array withBlock:block];
}

- (BOOL)isEmpty {
    return [_array count] == 0;
}

- (BOOL)isEqual:(JSList *)list {
    NSUInteger len = [_array count];
    NSUInteger i = 0;
    if (len != [list count]) {
        return NO;
    }
    for (i = 0; i < len; i++) {
        if (![_array[i] isEqual:[list nth:i]]) {
            return NO;
        }
    }
    return YES;
}

- (NSUInteger)hash {
    return [_array count];
}

/** Returns a new list which the reverse of the current list. */
- (JSList *)reverse {
    return [[JSList alloc] initWithArray:[[[[_array rest] reverseObjectEnumerator] allObjects] mutableCopy]];
}

/** Drops n elements. */
- (JSList *)drop:(NSInteger)n {
    NSMutableArray *arr = [_array mutableCopy];
    if (n > 0 && n <= [arr count]) {
        [arr removeObjectsInRange:NSMakeRange(0, n)];
        return [[JSList alloc] initWithArray:arr];
    }
    return nil;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSList alloc] initWithArray:_array];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
