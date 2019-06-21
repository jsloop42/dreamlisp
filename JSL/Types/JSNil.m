//
//  JSNil.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSNil.h"

@implementation JSNil {
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

+ (BOOL)isNil:(id)object {
    return [[object className] isEqual:[self className]];
}

- (instancetype)init {
    self = [super init];
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"nil";
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEqual:(id)object {
    return [JSNil isNil:object];
}

- (NSUInteger)hash {
    return 0;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [JSNil new];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [self dataTypeName];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, @"nil", _meta];
}

@end
