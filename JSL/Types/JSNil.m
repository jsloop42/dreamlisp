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
}

@synthesize meta = _meta;
@synthesize value;

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

- (BOOL)isEqual:(JSNil *)object {
    return [object isKindOfClass:[self class]];
}

- (NSUInteger)hash {
    return 0;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [JSNil new];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [JSNil new];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, @"nil", _meta];
}

@end