//
//  JSBool.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSBool.h"

@implementation JSBool {
    BOOL _flag;
    id<JSDataProtocol> _meta;
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

- (instancetype)initWithJSBool:(JSBool *)object {
    self = [super init];
    if (self) _flag = [object value];
    return self;
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

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEqual:(id)object {
    if (![JSBool isBool:object]) return NO;
    return _flag == [(JSBool *)object value];
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
    id elem = [[JSBool allocWithZone:zone] initWithBool:_flag];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSBool allocWithZone:zone] initWithBool:_flag];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"%@", _flag ? @"true" : @"false"];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _flag ? @"YES" : @"NO", _meta];
}

@end
