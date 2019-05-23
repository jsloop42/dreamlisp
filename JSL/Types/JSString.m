//
//  JSString.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSString.h"

@implementation JSString {
    NSString *_string;
    id<JSDataProtocol> _meta;
    NSInteger _position;
    BOOL _isImported;
}

@synthesize meta = _meta;
@synthesize value = _string;
@synthesize isImported = _isImported;

+ (BOOL)isString:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSString *)dataToString:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToString:data position:-1 fnName:fnName];
}

+ (JSString *)dataToString:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSString isString:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'string'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'string'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSString *)data;
}

- (instancetype)init {
    self = [super init];
    return self;
}

- (instancetype)initWithFormat:(NSString *)format, ... {
    self = [super init];
    if (self) {
        va_list args;
        va_start(args, format);
        _string = [[NSString alloc] initWithFormat:format arguments:args];
        va_end(args);
        self = [self initWithString:_string];
    }
    return self;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        _string = string;
    }
    return self;
}

- (instancetype)initWithContentsOfFile:(NSString *)filePath {
    self = [super init];
    if (self) {
        NSError *err = nil;
        _string = [[NSString alloc] initWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&err];
        if (!_string && err) [[[JSError alloc] initWithUserInfo:[err userInfo]] throw];
    }
    return self;
}

- (instancetype)initWithCString:(const char *)string {
    self = [super init];
    if (self) {
        _string = [[NSString alloc] initWithCString:string encoding:NSUTF8StringEncoding];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta string:(JSString *)string {
    self = [super init];
    if (self) {
        _string = [string value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"string";
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEmpty {
    return [_string count] == 0;
}

- (NSUInteger)count {
    return [_string length];
}

- (BOOL)isEqual:(JSString *)string {
    return [_string isEqualToString:[string value]];
}

- (NSUInteger)hash {
    return [_string hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[JSString alloc] initWithString:_string];
    [elem setIsImported:_isImported];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _string, _meta];
}

@end
