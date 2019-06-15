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
    BOOL _isImported;
    NSString *_moduleName;
}

@synthesize meta = _meta;
@synthesize value = _array;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;

+ (BOOL)isList:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isKindOfList:(id)object {
    return [object isKindOfClass:[self class]];
}

/** Checks if the given data is a list. Else throws exception with the given function name. */
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

- (id<JSDataProtocol> _Nullable)last {
    NSInteger count = [_array count] - 1;
    if (count < 0) return nil;
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
    return [[JSList alloc] initWithArray:[_array reverse]];
}

/** Drops n elements. */
- (JSList *)drop:(NSInteger)n {
    return [[JSList alloc] initWithArray:[[self value] drop:n]];
}

- (JSList *)sort:(NSInteger (*)(id, id, void *))sorter {
    return [[JSList alloc] initWithArray:[_array sortedArrayUsingFunction:sorter context:nil]];
}

- (JSList *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[JSList alloc] initWithArray:[_array sortedArrayUsingComparator:comparator]];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[JSList alloc] initWithArray:_array];
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
