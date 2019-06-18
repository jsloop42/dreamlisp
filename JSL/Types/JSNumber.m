//
//  JSNumber.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSNumber.h"

@implementation JSNumber {
    NSDecimalNumber *_n;
    NSString *_decimalPattern;
    NSRegularExpression *_decimalExp;
    BOOL _isDouble;
    id<JSDataProtocol> _meta;
    NSInteger _position;
    BOOL _isImported;
    NSString *_moduleName;
}

@synthesize meta = _meta;
@synthesize value = _n;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;

+ (BOOL)isNumber:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSNumber *)dataToNumber:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToNumber:data position:-1 fnName:fnName];
}

+ (JSNumber *)dataToNumber:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSNumber isNumber:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'number'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'number'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSNumber *)data;
}

+ (JSNumber *)dataToNumber:(id<JSDataProtocol>)data {
    return [self dataToNumber:data position:-1];
}

+ (JSNumber *)dataToNumber:(id<JSDataProtocol>)data position:(NSInteger)position {
    if (![self isNumber:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithArity, @"'number'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatch, @"'number'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSNumber *)data;
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

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta number:(JSNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        _n = [number value];
        _meta = meta;
        _isDouble = [self checkDouble:[_n stringValue]];
    }
    return self;
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

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
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
    if ([TypeUtils matchString:string withExpression:_decimalExp]) {
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
    if (![JSNumber isNumber:object]) return NO;
    return [_n isEqualToNumber:[(JSNumber *)object value]];
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
    id elem = [[JSNumber alloc] initWithNumber:_n];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _n, _meta];
}

@end
