//
//  DLList.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLList.h"

@implementation DLList {
    NSMutableArray *_array;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _array;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isList:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isKindOfList:(id)object {
    return [object isKindOfClass:[self class]];
}

/** Checks if the given data is a list. Else throws exception with the given function name. */
+ (DLList *)dataToList:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToList:data position:-1 fnName:fnName];
}

+ (DLList *)dataToList:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLList isKindOfList:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'list'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'list'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLList *)data;
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

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta list:(DLList *)list {
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

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (void)add:(id<DLDataProtocol>)object {
    [_array addObject:object];
}

- (void)add:(id<DLDataProtocol>)object atIndex:(NSUInteger)index {
    [_array insertObject:object atIndex:index];
}

- (DLList *)addObject:(id<DLDataProtocol>)object {
    DLList *xs = [self copy];
    [(NSMutableArray *)[xs value] addObject:object];
    return xs;
}

- (void)addObjectsFromArray:(NSMutableArray *)array {
    [_array addObjectsFromArray:array];
}

- (void)addObjectsFromList:(DLList *)list {
    [_array addObjectsFromArray:[list value]];
}

- (void)update:(id<DLDataProtocol>)object atIndex:(NSUInteger)index {
    [_array replaceObjectAtIndex:index withObject:object];
}

- (void)remove:(id<DLDataProtocol>)object {
    [_array removeObject:object];
}

- (void)removeAtIndex:(NSUInteger)index {
    [_array removeObjectAtIndex:index];
}

- (NSUInteger)count {
    return [_array count];
}

- (id<DLDataProtocol>)first {
    return [_array firstObject];
}

- (id<DLDataProtocol>)second {
    return [_array objectAtIndex:1];
}

- (id<DLDataProtocol>)rest {
    NSMutableArray *arr = [_array mutableCopy];
    [arr removeObjectAtIndex:0];
    return [[DLList alloc] initWithArray:arr];
}

- (id<DLDataProtocol> _Nullable)last {
    NSInteger count = [_array count] - 1;
    if (count < 0) return nil;
    return [_array objectAtIndex:[_array count] - 1];
}

- (id<DLDataProtocol>)dropLast {
    NSMutableArray *arr = [_array mutableCopy];
    [arr removeLastObject];
    return [[DLList alloc] initWithArray:arr];
}

- (id<DLDataProtocol>)nth:(NSInteger)n {
    return [_array objectAtIndex:n];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    @autoreleasepool {
        return [TypeUtils mapOnArray:_array withBlock:block];
    }
}

- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block; {
    [_array enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:block];
}

- (BOOL)isEmpty {
    return [_array count] == 0;
}

- (BOOL)isEqual:(id)object {
    if (![DLList isKindOfList:object]) return NO;
    DLList *list = (DLList *)object;
    NSUInteger len = [_array count];
    NSUInteger i = 0;
    if (len != [list count]) return NO;
    for (i = 0; i < len; i++) {
        if (![_array[i] isEqual:[list nth:i]]) return NO;
    }
    return YES;
}

- (NSUInteger)hash {
    return [_array count];
}

/** Returns a new list which the reverse of the current list. */
- (DLList *)reverse {
    return [[DLList alloc] initWithArray:[_array reverse]];
}

/** Drops n elements. */
- (DLList *)drop:(NSInteger)n {
    return [[DLList alloc] initWithArray:[[self value] drop:n]];
}

- (DLList *)sort:(NSInteger (*)(id, id, void *))sorter {
    return [[DLList alloc] initWithArray:[_array sortedArrayUsingFunction:sorter context:nil]];
}

- (DLList *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[DLList alloc] initWithArray:[_array sortedArrayUsingComparator:comparator]];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (NSMutableArray *)subArrayWithStartIndex:(NSInteger)start endIndex:(NSInteger)end {
    if (start > end) return [NSMutableArray new];
    if (end > [self count]) return _array;
    return [[_array subarrayWithRange:NSMakeRange(start, end - start + 1)] mutableCopy];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLList alloc] initWithArray:_array];
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
    return [NSString stringWithFormat:@"(%@)", str];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_array description], _meta];
}

@end
