//
//  DLSet.m
//  DreamLisp
//
//  Created by Jaseem V V on 24.07.2024.
//  Copyright © 2024 DreamLisp. All rights reserved.
//

#import "DLSet.h"

@implementation DLSet {
    NSMutableSet *_set;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _set;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isSet:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLSet *)dataToSet:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToSet:data position:-1 fnName:fnName];
}

/** Type checks if the given data is of DLSet, else throws an exception. */
+ (DLSet *)dataToSet:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLSet isSet:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'set'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'set'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLSet *)data;
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithSet:(NSMutableSet *)set {
    self = [super init];
    if (self) {
        _set = set;
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)array {
    self = [super init];
    if (self) {
        [self bootstrap];
        [self fromArray:array];
    }
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta set:(DLSet *)set {
    self = [super init];
    if (self) {
        _set = [set value];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        [self bootstrap];
        _set = [coder decodeObjectOfClass:[self class] forKey:@"DLSet_set"];
        _moduleName = [coder decodeObjectOfClass:[self class] forKey:@"DLSet_moduleName"];
        _meta = [coder decodeObjectOfClass:[self class] forKey:@"DLSet_meta"];
        _position = [[coder decodeObjectOfClass:[self class] forKey:@"DLSet_position"] integerValue];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self class] forKey:@"DLSet_isMutable"];
        [isMutableValue getValue:&_isMutable];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self class] forKey:@"DLSet_isImported"];
        [isImportedValue getValue:&_isImported];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_set forKey:@"DLSet_set"];
    [coder encodeObject:_moduleName forKey:@"DLSet_moduleName"];
    [coder encodeObject:_meta forKey:@"DLSet_meta"];
    [coder encodeObject:@(_position) forKey:@"DLSet_position"];
    NSValue *isMutableValue = [NSValue valueWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLSet_isMutable"];
    NSValue *isImportedValue = [NSValue valueWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLSet_isImported"];
}

- (Class)classForCoder {
    return [self class];
}

- (void)bootstrap {
    _set = [[NSMutableSet alloc] init];
}

- (void)fromArray:(NSArray *)array {
    [_set removeAllObjects];
    _set = [NSMutableSet setWithArray:array];
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"set";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSUInteger)count {
    return [_set count];
}

- (BOOL)isEmpty {
    return [_set isEmpty];
}

- (BOOL)contains:(id<DLDataProtocol>)object {
    return [_set member:object] != nil;
}

- (NSMutableSet *)removeImmutable:(id<DLDataProtocol>)object {
    NSMutableSet *set = [_set mutableCopyWithZone:nil];
    [set removeObject:object];
    return set;
}

- (BOOL)isEqual:(id)object {
    if (![DLSet isSet:object]) return NO;
    DLSet *set = (DLSet *)object;
    return [_set isEqualTo:[set value]];
}

- (NSUInteger)hash {
    return [_set hash];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLSet alloc] initWithSet:_set];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    NSMutableArray *arr = [[NSMutableArray alloc] init];
    for (id elem in _set) {
        [arr addObject:[elem description]];
    }
    return [arr isEmpty] ? @"#{}" : [NSString stringWithFormat:@"#{%@}", [arr componentsJoinedByString:@" "]];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, [_set description], _meta];
}

@end
