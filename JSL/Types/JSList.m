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
    id<JSDataProtocol> _meta;
    NSInteger _position;
}

@synthesize meta = _meta;
@synthesize value = _array;

+ (BOOL)isList:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isKindOfList:(id)object {
    return [object isKindOfClass:[self class]];
}

+ (JSList *)dataToList:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToList:data position:-1 fnName:fnName];
}

+ (JSList *)dataToList:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSList isKindOfList:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'list'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'list'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSList *)data;
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

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta list:(JSList *)list {
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

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (void)add:(id<JSDataProtocol>)object {
    [_array addObject:object];
}

- (void)add:(id<JSDataProtocol>)object atIndex:(NSUInteger)index {
    [_array insertObject:object atIndex:index];
}

- (void)update:(id<JSDataProtocol>)object atIndex:(NSUInteger)index {
    [_array replaceObjectAtIndex:index withObject:object];
}

- (void)remove:(id<JSDataProtocol>)object {
    [_array removeObject:object];
}

- (void)removeAtIndex:(NSUInteger)index {
    [_array removeObjectAtIndex:index];
}

- (NSUInteger)count {
    return [_array count];
}

- (id<JSDataProtocol>)first {
    return [_array firstObject];
}

- (id<JSDataProtocol>)second {
    return [_array objectAtIndex:1];
}

- (id<JSDataProtocol>)rest {
    NSMutableArray *arr = [_array mutableCopy];
    [arr removeObjectAtIndex:0];
    return [[JSList alloc] initWithArray:arr];
}

- (id<JSDataProtocol>)last {
    return [_array objectAtIndex:[_array count] - 1];
}

- (id<JSDataProtocol>)dropLast {
    NSMutableArray *arr = [_array mutableCopy];
    [arr removeLastObject];
    return [[JSList alloc] initWithArray:arr];
}

- (id<JSDataProtocol>)nth:(NSInteger)n {
    return [_array objectAtIndex:n];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:_array withBlock:block];
}

- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block; {
    [_array enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:block];
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
- (JSList * _Nullable)drop:(NSInteger)n {
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
    return [[JSList alloc] initWithArray:_array];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSList alloc] initWithArray:_array];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
