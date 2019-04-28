//
//  JSNumber.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSNumber.h"

@implementation JSNumber {
    NSDecimalNumber *n;
    NSString *decimalPattern;
    BOOL _isDouble;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)initWithDouble:(double)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithDouble:number];
        _isDouble = YES;
    }
    return self;
}

- (instancetype)initWithInt:(int)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithInt:number];
        _isDouble = NO;
    }
    return self;
}

- (instancetype)initWithInteger:(NSUInteger)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithInteger:number];
        _isDouble = NO;
    }
    return self;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithString:string];
        _isDouble = [self checkDouble:[n stringValue]];
    }
    return self;
}

- (instancetype)initWithNumber:(NSDecimalNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = number;
        _isDouble = [self checkDouble:[n stringValue]];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta number:(JSNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [number value];
        _meta = meta;
        _isDouble = [self checkDouble:[n stringValue]];
    }
    return self;
}


- (void)bootstrap {
    decimalPattern = @"\\d+(\\.\\d+)";
}

- (NSString *)dataType {
    return [self className];
}

- (double)doubleValue {
    return [n doubleValue];
}

- (int)intValue {
    return [n intValue];
}

- (NSUInteger)integerValue {
    return [n integerValue];
}

- (NSDecimalNumber *)value {
    return n;
}

- (BOOL)checkDouble:(NSString *)string {
    if ([Utils matchString:string withPattern:decimalPattern]) {
        return YES;
    }
    return NO;
}

- (BOOL)isDouble {
    return _isDouble;
}

- (NSString *)string {
    if (_isDouble && ![self checkDouble:[n stringValue]]) {
        return [NSString stringWithFormat:@"%.01f", [n doubleValue]];
    }
    return [n stringValue];
}

- (BOOL)isEqual:(JSNumber *)number {
    return [n isEqualToNumber:[number value]];
}
- (NSUInteger)hash {
    return [n hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSNumber alloc] initWithNumber:n];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, n, _meta];
}

@end
