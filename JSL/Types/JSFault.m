//
//  JSFault.m
//  JSL
//
//  Created by jsloop on 19/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSFault.h"

/** Fault class which is used as a placeholder object for module exports and imports. Faults are resolved when accessed for the first time. */
@implementation JSFault {
    id<JSDataProtocol> _meta;
    NSInteger _position;
    NSString *_value;
}

@synthesize meta = _meta;
@synthesize value = _value;

+ (BOOL)isFault:(id)object {
    return [[object className] isEqual:[self className]];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _value = [[[NSUUID UUID] UUIDString] lowercaseString];
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"fault";
}

- (BOOL)hasMeta {
    return NO;
}

- (NSInteger)position {
    return _position;
}

- (NSUInteger)hash {
    return [_value hash];
}

- (BOOL)isEqual:(id)object {
    if (![JSFault isFault:object]) return NO;
    return [_value isEqual:[(JSFault *)object value]];
}

- (nonnull id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    JSFault *fault = [JSFault new];
    [fault setValue:_value];
    return fault;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    JSFault *fault = [JSFault new];
    [fault setValue:_value];
    return fault;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@>", NSStringFromClass([self class]), self, _value];
}

@end
