//
//  DLAtom.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLAtom.h"

@implementation DLAtom {
    id<DLDataProtocol> _data;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _data;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isAtom:(id)object {
    return [[object class] isEqual:[self class]];
}

+ (DLAtom *)dataToAtom:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToAtom:data position:-1 fnName:fnName];
}

+ (DLAtom *)dataToAtom:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLAtom isAtom:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'atom'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'atom'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLAtom *)data;
}

- (void)dealloc {
    [DLLog debug:@"DLAtom dealloc"];
}

- (instancetype)initWithData:(id<DLDataProtocol>)data {
    self = [super init];
    if (self) _data = data;
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta atom:(DLAtom *)atom {
    self = [super init];
    if (self) {
        _data = [atom value];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _data = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLAtom_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLAtom_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLAtom_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLAtom_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLAtom_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLAtom_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_data forKey:@"DLAtom_value"];
    [coder encodeObject:_meta forKey:@"DLAtom_meta"];
    [coder encodeObject:_moduleName forKey:@"DLAtom_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLAtom_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLAtom_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLAtom_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (NSString *)dataType {
    return NSStringFromClass([self class]);
}

- (NSString *)dataTypeName {
    return @"atom";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEqual:(id)object {
    if (![DLAtom isAtom:object]) return NO;
    return [[(DLAtom *)object value] isEqual:_data];
}

- (NSUInteger)hash {
    return [_data hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (NSInteger)sortValue {
    return [_data sortValue];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLAtom alloc] initWithData:_data];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[DLAtom allocWithZone:zone] initWithData:_data];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"(atom %@)", _data];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _data, _meta];
}

@end
