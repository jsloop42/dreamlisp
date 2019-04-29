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
    BOOL _isDouble;
    JSData *_meta;
}

@synthesize meta = _meta;

+ (BOOL)isNumber:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSNumber *)dataToNumber:(JSData *)data {
    return [self dataToNumber:data position:-1];
}

+ (JSNumber *)dataToNumber:(JSData *)data position:(NSInteger)position {
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

- (instancetype)initWithInteger:(NSUInteger)number {
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


- (instancetype)initWithMeta:(JSData *)meta number:(JSNumber *)number {
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
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"number";
}

- (double)doubleValue {
    return [_n doubleValue];
}

- (int)intValue {
    return [_n intValue];
}

- (NSUInteger)integerValue {
    return [_n integerValue];
}

- (NSDecimalNumber *)value {
    return _n;
}

- (BOOL)checkDouble:(NSString *)string {
    if ([Utils matchString:string withPattern:_decimalPattern]) {
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

- (BOOL)isEqual:(JSNumber *)number {
    return [_n isEqualToNumber:[number value]];
}
- (NSUInteger)hash {
    return [_n hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSNumber alloc] initWithNumber:_n];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _n, _meta];
}

@end
