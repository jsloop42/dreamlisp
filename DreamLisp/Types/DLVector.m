//
//  DLVector.m
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLVector.h"

@implementation DLVector {
    //NSMutableArray *_array;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    BOOL _isImported;
    NSString *_moduleName;
}

@synthesize meta = _meta;
@dynamic value;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;

+ (BOOL)isVector:(id)object {
    return [[object class] isEqual:[self class]];
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
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'list' or 'vector'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'list' or 'vector'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLList *)data;
}

- (void)dealloc {
    [DLLog debug:@"DLVector dealloc"];
}

- (instancetype)init {
    return [super init];
}

- (instancetype)initWithArray:(NSArray *)list {
    self = [super init];
    if (self) {
        [super setValue:[list mutableCopy]];
    }
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta vector:(DLVector *)vector {
    self = [super init];
    if (self) {
        [super setValue:[vector value]];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        self.value = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLVector_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLVector_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLVector_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLVector_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLVector_isImported"];
        [isImportedValue getValue:&_isImported];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:[self value] forKey:@"DLVector_value"];
    [coder encodeObject:_meta forKey:@"DLVector_meta"];
    [coder encodeObject:_moduleName forKey:@"DLVector_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLVector_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLVector_isImported"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (NSString *)dataType {
    return NSStringFromClass(self.class);
}

- (NSString *)dataTypeName {
    return @"vector";
}

- (NSInteger)position {
    return _position;
}

- (id)value {
    return [super value];
}

- (void)setValue:(id)value {
    [super setValue:value];
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [DLTypeUtils mapOnArray:self.value withBlock:block];
}

- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block {
    [self.value enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:block];
}

- (DLVector *)addObject:(id<DLDataProtocol>)object {
    DLVector *vec = [self copy];
    [(NSMutableArray *)[vec value] addObject:object];
    return vec;
}

- (void)appendObject:(id<DLDataProtocol>)object {
    [(NSMutableArray *)self.value addObject:object];
}

- (DLList *)list {
    return [[DLList alloc] initWithArray:self.value];
}

/** Returns a new list which the reverse of the current list. */
- (DLVector *)reverse {
    return [[DLVector alloc] initWithArray:[self.value reverse]];
}

/** Drops n elements. */
- (DLVector *)drop:(NSInteger)n {
    return [[DLVector alloc] initWithArray:[[self value] drop:n]];
}

- (BOOL)isEqual:(id)object {
    if (![DLList isKindOfList:object]) return NO;
    DLVector *vector = (DLVector *)object;
    NSUInteger len = [self.value count];
    NSUInteger i = 0;
    if (len != [vector count]) return NO;
    for (i = 0; i < len; i++) {
        if (![self.value[i] isEqual:[vector nth:i]]) return NO;
    }
    return YES;
}

- (NSUInteger)hash {
    return [self.value count];
}

- (DLVector *)sort:(NSInteger (*)(id, id, void *))sorter {
    return [[DLVector alloc] initWithArray:[self.value sortedArrayUsingFunction:sorter context:nil]];
}

- (DLVector *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    return [[DLVector alloc] initWithArray:[self.value sortedArrayUsingComparator:comparator]];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (void)forwardInvocation:(NSInvocation *)anInvocation {
    if ([self respondsToSelector:anInvocation.selector]) {
        [anInvocation invokeWithTarget:self];
    } else if ([self.value respondsToSelector:anInvocation.selector]) {
        [anInvocation invokeWithTarget:self.value];
    } else {
        @throw [DLError exceptionWithFormat:DLUnrecognizedSelectorError, self, anInvocation.selector];
    }
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector {
    if ([self respondsToSelector:aSelector]) return [self methodSignatureForSelector:aSelector];
    if ([self.value respondsToSelector:aSelector]) return [self.value methodSignatureForSelector:aSelector];
    @throw [DLError exceptionWithFormat:DLUnrecognizedSelectorError, self, aSelector];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLVector alloc] initWithArray:self.value];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    NSMutableString *str = [NSMutableString new];
    NSUInteger len = [self.value count];
    NSUInteger last = len - 1;
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        [str appendString:[self.value[i] description]];
        if (i != last) [str appendString:@" "];
    }
    return [[NSString alloc] initWithFormat:@"[%@]", str];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [self.value description], _meta];
}

@end
