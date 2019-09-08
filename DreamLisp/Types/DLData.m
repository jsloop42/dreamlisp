//
//  DLData.m
//  DreamLisp
//
//  Created by jsloop on 07/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "DLData.h"

@implementation DLData {
    NSData *_data;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _data;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isData:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLData *)dataToData:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToData:data position:-1 fnName:fnName];
}

+ (DLData *)dataToData:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLData isData:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'data'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'data'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLData *)data;
}

- (instancetype)initWithData:(NSData *)data {
    self = [super init];
    if (self) {
        _data = data;
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithDLData:(DLData *)data {
    self = [super init];
    if (self) {
         _data = [data value];
        [self bootstrap];
        _isImported = [data isImported];
        _isMutable = [data isMutable];
        _moduleName = [data moduleName];
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _data = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLData_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_data forKey:@"DLData_value"];
    [coder encodeObject:_meta forKey:@"DLData_meta"];
    [coder encodeObject:_moduleName forKey:@"DLData_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLData_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLData_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLData_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _moduleName = [State currentModuleName];
    _isImported = NO;
    _isMutable = NO;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"data";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEqual:(id)object {
    if (![DLData isData:object]) return NO;
    DLData *data = (DLData *)object;
    return [[data value] isEqual:_data];
}

- (NSUInteger)hash {
    return [_data hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLData allocWithZone:zone] initWithData:_data];
    [elem setIsImported:_isImported];
    [elem setIsMutable:_isMutable];
    [elem setMeta:_meta];
    [elem setModuleName:_moduleName];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [_data description];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@>", NSStringFromClass([self class]), self, [self description]];
}

@end
