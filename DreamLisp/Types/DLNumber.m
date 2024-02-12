//
//  DLNumber.m
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLNumber.h"

@implementation DLNumber {
    NSDecimalNumber *_n;
    NSString *_decimalPattern;
    NSRegularExpression *_decimalExp;
    BOOL _isDouble;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _n;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isNumber:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToNumber:data position:-1 fnName:fnName];
}

+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLNumber isNumber:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'number'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'number'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLNumber *)data;
}

+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data {
    return [self dataToNumber:data position:-1];
}

+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data position:(NSInteger)position {
    if (![self isNumber:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithArity, @"'number'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatch, @"'number'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLNumber *)data;
}

- (instancetype)initWithDouble:(double)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = [[NSDecimalNumber alloc] initWithDouble:number];
        _isDouble = YES;
    }
    return self;
}

- (instancetype)initWithInt:(int)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = [[NSDecimalNumber alloc] initWithInt:number];
        _isDouble = NO;
    }
    return self;
}

- (instancetype)initWithInteger:(NSInteger)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = [[NSDecimalNumber alloc] initWithInteger:number];
        _isDouble = NO;
    }
    return self;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = [[NSDecimalNumber alloc] initWithString:string];
        _isDouble = [self checkDouble:string];
    }
    return self;
}

- (instancetype)initWithNumber:(NSDecimalNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = number;
        _isDouble = [self checkDouble:[_n stringValue]];
    }
    return self;
}

- (instancetype)initWithDoubleNumber:(NSDecimalNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = number;
        _isDouble = YES;
    }
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta number:(DLNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = [number value];
        _meta = meta;
        _isDouble = [self checkDouble:[_n stringValue]];
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _n = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_value"];
        NSValue *isDoubleValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_isDouble"];
        [isDoubleValue getValue:&_isDouble];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLNumber_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_n forKey:@"DLNumber_value"];
    NSValue *isDoubleValue = [[NSValue alloc] initWithBytes:&_isDouble objCType:@encode(BOOL)];
    [coder encodeObject:isDoubleValue forKey:@"DLNumber_isDouble"];
    [coder encodeObject:_meta forKey:@"DLNumber_meta"];
    [coder encodeObject:_moduleName forKey:@"DLNumber_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLNumber_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLNumber_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLNumber_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _decimalPattern = @"\\d+(\\.\\d+)";
    _decimalExp = [NSRegularExpression regularExpressionWithPattern:_decimalPattern options:0 error:nil];
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"number";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (double)doubleValue {
    return [_n doubleValue];
}

- (int)intValue {
    return [_n intValue];
}

- (NSInteger)integerValue {
    return [_n integerValue];
}

- (BOOL)checkDouble:(NSString *)string {
    if ([DLTypeUtils matchString:string withExpression:_decimalExp]) {
        return YES;
    }
    return NO;
}

- (BOOL)isDouble {
    return _isDouble;
}

- (NSString *)string {
    if (_isDouble && ![self checkDouble:[_n stringValue]]) {
        return [NSString stringWithFormat:@"%.01f", [_n doubleValue]];
    }
    return [_n stringValue];
}

- (BOOL)isEqual:(id)object {
    if (![DLNumber isNumber:object]) return NO;
    return [_n isEqualToNumber:[(DLNumber *)object value]];
}

- (NSUInteger)hash {
    return [_n hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (NSInteger)sortValue {
    return _isDouble ? [_n doubleValue] : [_n integerValue];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLNumber alloc] initWithNumber:_n];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [self string];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _n, _meta];
}

@end
