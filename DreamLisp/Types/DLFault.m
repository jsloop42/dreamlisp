//
//  DLFault.m
//  DreamLisp
//
//  Created by jsloop on 19/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLFault.h"

/** Fault class which is used as a placeholder object for module exports and imports. Faults are resolved when accessed for the first time. */
@implementation DLFault {
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_value;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _value;
@synthesize moduleName = _moduleName;
@synthesize isImported = _isImported;
@synthesize isMutable = _isMutable;

+ (BOOL)isFault:(id)object {
    return [[object className] isEqual:[self className]];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithModule:(NSString *)moduleName isImportFault:(BOOL)isImportFault  {
    self = [super init];
    if (self) {
        [self bootstrap];
        _moduleName = moduleName;
        _isImported = isImportFault;
    }
    return self;
}

- (void)bootstrap {
    _value = [[[NSUUID UUID] UUIDString] lowercaseString];
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
    if (![DLFault isFault:object]) return NO;
    return [_value isEqual:[(DLFault *)object value]];
}

- (nonnull id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLFault *fault = [DLFault new];
    [fault setValue:_value];
    [fault setIsImported:_isImported];
    return fault;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"%@", _value];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@>", NSStringFromClass([self class]), self, _value];
}

@end
