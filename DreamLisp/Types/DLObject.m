//
//  DLObject.m
//  DreamLisp
//
//  Created by jsloop on 01/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLObject.h"

@implementation DLObject {
    id _object;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _object;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isObject:(id)any {
    return [[any className] isEqual:[self className]];
}

- (void)dealloc {
    [DLLog debug:[NSString stringWithFormat:@"%@ dealloc", [self className]]];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithObject:(id)object {
    self = [super init];
    if (self) {
        [self bootstrap];
        _object = object;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _object = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_value"];
        _cls = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLObject_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_object forKey:@"DLObject_value"];
    [coder encodeObject:_cls forKey:@"DLObject_cls"];
    [coder encodeObject:_meta forKey:@"DLObject_meta"];
    [coder encodeObject:_moduleName forKey:@"DLKeyword_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLKeyword_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLKeyword_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLKeyword_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}


- (void)bootstrap {
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"object";
}

- (BOOL)isEqual:(id)object {
    if (![DLObject isObject:object]) return NO;
    DLObject *obj = (DLObject *)object;
    return [obj.value isEqual:_object];
}

- (NSUInteger)hash {
    return [_object hash];
}

- (BOOL)hasMeta {
    return NO;
}

- (NSInteger)position {
    return _position;
}

- (nonnull id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLObject *obj = [DLObject new];
    obj.value = _object;
    obj.isMutable = _isMutable;
    obj.isImported = _isImported;
    obj.meta = _meta;
    obj.moduleName = _moduleName;
    return obj;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [_object description];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %@ %p>", [self dataTypeName], [self description], self];
}

@end
