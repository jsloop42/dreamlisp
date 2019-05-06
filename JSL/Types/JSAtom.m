//
//  JSAtom.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSAtom.h"

@implementation JSAtom {
    id<JSDataProtocol> _data;
    id<JSDataProtocol> _meta;
    NSInteger _position;
}

@synthesize meta = _meta;

+ (BOOL)isAtom:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSAtom *)dataToAtom:(id<JSDataProtocol>)data {
    return [self dataToAtom:data position:-1];
}

+ (JSAtom *)dataToAtom:(id<JSDataProtocol>)data position:(NSInteger)position {
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

- (instancetype)initWithData:(id<JSDataProtocol>)data {
    self = [super init];
    if (self) _data = data;
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta atom:(JSAtom *)atom {
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

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (id<JSDataProtocol>)value {
    return _data;
}

- (void)setValue:(id<JSDataProtocol>)data {
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
    return [[JSAtom alloc] initWithData:_data];    
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSAtom allocWithZone:zone] initWithData:_data];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _data, _meta];
}

@end