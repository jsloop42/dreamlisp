//
//  DLKeyword.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLKeyword.h"

@implementation DLKeyword {
    NSString *_string;
    id<DLDataProtocol> _meta;
    NSInteger _position;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize value = _string;
@synthesize meta = _meta;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;
@synthesize isMutable = _isMutable;

+ (BOOL)isKeyword:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isEncodedKeyword:(id)object {
    return [object isKindOfClass:[NSString class]] && [[object substringToIndex:1] isEqual:@"\u029e"];
}

+ (DLKeyword *)dataToKeyword:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToKeyword:data position:-1 fnName:fnName];
}

+ (DLKeyword *)dataToKeyword:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLKeyword isKeyword:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'keyword'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'keyword'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLKeyword *)data;
}

+ (DLKeyword *)keywordWithString:(NSString *)string {
    return [[DLKeyword alloc] initWithString:string];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        self = [self initWithKeyword:@""];
    }
    return self;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        _string = [string stringByReplacingCharactersInRange:NSMakeRange(0, 0) withString:@":"];
    }
    return self;
}

- (instancetype)initWithKeyword:(NSString *)string {
    self = [super init];
    if (self) {
        _string = string;
    }
    return self;
}

- (instancetype)initWithEncodedKeyword:(NSString *)keyword {
    self = [super init];
    if (self) {
        if ([[keyword substringToIndex:1] isEqual:@"\u029e"]) {
            _string = [keyword stringByReplacingCharactersInRange:NSMakeRange(0, 1) withString:@""];
        } else {
            _string = keyword;
        }
    }
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta keyword:(DLKeyword *)keyword {
    self = [super init];
    if (self) {
        _string = [keyword value];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _string = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLKeyword_value"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLKeyword_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLKeyword_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLKeyword_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLKeyword_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLKeyword_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_string forKey:@"DLKeyword_value"];
    [coder encodeObject:_meta forKey:@"DLKeyword_meta"];
    [coder encodeObject:_moduleName forKey:@"DLKeyword_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLKeyword_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLKeyword_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLKeyword_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"keyword";
}

- (NSInteger)position {
    return _position;
}

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSString *)string {
    if ([_string characterAtIndex:0] == ':') {
        return [_string stringByReplacingCharactersInRange:NSMakeRange(0, 1) withString:@""];
    }
    return _string;
}

- (NSString *)encoded {
    return [_string stringByReplacingCharactersInRange:NSMakeRange(0, 0) withString:@"\u029e"];
}

- (NSString *)decoded {
    if ([[_string substringToIndex:1] isEqual:@"\u029e"]) {
        return [_string substringFromIndex:1];
    }
    return _string;
}

/** Checks if the given string is same as the keyword's underlying keyword string. */
- (BOOL)isEqualToString:(id)string {
    return [[self string] isEqual:string];
}

- (BOOL)isEqual:(id)object {
    if (![DLKeyword isKeyword:object]) return NO;
    return [_string isEqualToString:[(DLKeyword *)object value]];
}

- (NSUInteger)hash {
    return [_string hash];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLKeyword alloc] initWithKeyword:[self string]];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return _string;
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _string, _meta];
}

@end
