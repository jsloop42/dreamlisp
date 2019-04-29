//
//  JSAtom.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSAtom.h"

@implementation JSAtom {
    JSData *_data;
    JSData *_meta;
}

@synthesize meta = _meta;

+ (BOOL)isAtom:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSAtom *)dataToAtom:(JSData *)data {
    return [self dataToAtom:data position:-1];
}

+ (JSAtom *)dataToAtom:(JSData *)data position:(NSInteger)position {
    if (![JSAtom isAtom:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithArity, @"'atom'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatch, @"'atom'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSAtom *)data;
}

- (instancetype)initWithData:(JSData *)data {
    self = [super init];
    if (self) {
        _data = data;
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta atom:(JSAtom *)atom {
    self = [super init];
    if (self) {
        _data = [atom value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"atom";
}

- (JSData *)value {
    return _data;
}

- (void)setValue:(JSData *)data {
    _data = data;
}

- (BOOL)isEqual:(JSAtom *)atom {
    return [[atom value] isEqual:_data];
}

- (NSUInteger)hash {
    return [_data hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSAtom alloc] initWithData:_data];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _data, _meta];
}

@end
