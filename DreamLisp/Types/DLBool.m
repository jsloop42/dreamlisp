//
//  DLBool.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLBool.h"

@implementation DLBool {
    BOOL _flag;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isBool:(id)object {
    return [[object className] isEqual:[self className]];
}

- (instancetype)initWithBool:(BOOL)flag {
    self = [super init];
    if (self) _flag = flag;
    return self;
}

- (instancetype)initWithDLBool:(DLBool *)object {
    self = [super init];
    if (self) _flag = [object value];
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        NSValue *flagValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLBool_value"];
        [flagValue getValue:&_flag];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLBool_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLBool_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLBool_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLBool_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLBool_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    NSValue *_flagValue = [[NSValue alloc] initWithBytes:&_flag objCType:@encode(BOOL)];
    [coder encodeObject:_flagValue forKey:@"DLBool_value"];
    [coder encodeObject:_meta forKey:@"DLBool_meta"];
    [coder encodeObject:_moduleName forKey:@"DLBool_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLBool_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLBool_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLBool_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"bool";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEqual:(id)object {
    if (![DLBool isBool:object]) return NO;
    return _flag == [(DLBool *)object value];
}

- (NSUInteger)hash {
    return _flag ? 1 : 0;
}

- (BOOL)value {
    return _flag;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLBool allocWithZone:zone] initWithBool:_flag];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[DLBool allocWithZone:zone] initWithBool:_flag];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"%@", _flag ? @"true" : @"false"];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _flag ? @"YES" : @"NO", _meta];
}

@end
