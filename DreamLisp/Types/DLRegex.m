//
//  DLRegex.m
//  DreamLisp
//
//  Created by Jaseem V V on 07/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLRegex.h"

@implementation DLRegex {
    NSRegularExpression *_regex;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value = _regex;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isRegex:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLRegex *)dataToRegex:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToRegex:data position:-1 fnName:fnName];
}

+ (DLRegex *)dataToRegex:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLRegex isRegex:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'regex' or a 'string' pattern", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'regex' or a 'string' pattern", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLRegex *)data;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        _regex = [NSRegularExpression regularExpressionWithPattern:string options:0 error:nil];
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithRegularExpression:(NSRegularExpression *)regex {
    self = [super init];
    if (self) {
        _regex = regex;
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _regex = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLRegex_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLRegex_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLRegex_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLRegex_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLRegex_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLRegex_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_regex forKey:@"DLRegex_regex"];
    [coder encodeObject:_meta forKey:@"DLRegex_meta"];
    [coder encodeObject:_moduleName forKey:@"DLRegex_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLRegex_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLRegex_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLRegex_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _moduleName = [DLState currentModuleName];
    _isImported = NO;
    _isMutable = NO;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"regex";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEqual:(id)object {
    if (![DLRegex isRegex:object]) return NO;
    return [[(DLRegex *)object value] isEqual:_regex];
}

- (NSUInteger)hash {
    return [_regex hash];
}

- (BOOL)hasMeta {
    return NO;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLRegex *regex = [[DLRegex allocWithZone:zone] initWithRegularExpression:_regex];
    regex.position = _position;
    regex.moduleName = _moduleName;
    regex.meta = _meta;
    regex.isImported = _isImported;
    regex.isMutable = _isMutable;
    return regex;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [_regex description];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@>", NSStringFromClass([self class]), self, [self description]];
}

@end
