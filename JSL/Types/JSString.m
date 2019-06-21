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
    NSMutableString *_mstring;
    id<JSDataProtocol> _meta;
    NSInteger _position;
    BOOL _isImported;
    NSString *_moduleName;
    BOOL _isMutable;
}

@synthesize meta = _meta;
@synthesize value;
@synthesize isMutable = _isMutable;
@synthesize isImported = _isImported;
@synthesize moduleName = _moduleName;

+ (BOOL)isString:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isString:(id)object withValue:(NSString *)name {
    if (![self isString:object]) return false;
    return [[(JSString *)object value] isEqual:name];
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

+ (JSString *)mutable {
    return [[JSString alloc] initWithMutableString];
}

- (instancetype)init {
    self = [super init];
    return self;
}

- (instancetype)initWithFormat:(NSString *)format, ... {
    self = [super init];
    if (self) {
        [self bootstrap];
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
        [self bootstrap];
        _string = string;
    }
    return self;
}

- (instancetype)initWithMutableString {
    return [self initWithMutableString:[NSMutableString new]];
}

- (instancetype)initWithMutableString:(NSMutableString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        _mstring = string;
        _isMutable = YES;
    }
    return self;
}

- (instancetype)initWithContentsOfFile:(NSString *)filePath {
    self = [super init];
    if (self) {
        [self bootstrap];
        NSError *err = nil;
        _string = [[NSString alloc] initWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&err];
        if (!_string && err) [[[JSError alloc] initWithUserInfo:[err userInfo]] throw];
    }
    return self;
}

- (instancetype)initWithCString:(const char *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        _string = [[NSString alloc] initWithCString:string encoding:NSUTF8StringEncoding];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta string:(JSString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        _string = [string value];
        _meta = meta;
    }
    return self;
}

- (void)bootstrap {
    _isMutable = NO;
    _isImported = NO;
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

- (NSString *)value {
    return _string ? _string : _mstring;
}

- (NSMutableString *)mutableValue {
    return _mstring;
}

- (NSString *)substringFrom:(NSInteger)start {
    NSUInteger len = [self count];
    NSInteger end = len - start;
    if (start > len) return @"";
    return [self substringFrom:start count:len - start];
}

- (NSString *)substringFrom:(NSInteger)start to:(NSInteger)end {
    NSString *str = [self value];
    if (!str) return @"";
    NSUInteger len = [str count];
    if (end >= len) [[[JSError alloc] initWithFormat:IndexOutOfBounds, end, len] throw];
    if (end < start) [[[JSError alloc] initWithFormat:IndexOutOfBounds, end, start] throw];
    return [str substringWithRange:NSMakeRange(start, end - start + 1)];
}

- (NSString *)substringFrom:(NSInteger)start count:(NSInteger)count {
    NSString *str = [self value];
    if (!str) return @"";
    NSUInteger len = [str count];
    if (count > len) [[[JSError alloc] initWithFormat:IndexOutOfBounds, count, count < 0 ? 0 : len] throw];
    return [str substringWithRange:NSMakeRange(start, count)];
}

- (void)setValue:(id)value {
    _string = value;
}

- (void)setMutableValue:(NSMutableString *)mutableValue {
    _mstring = mutableValue;
}

- (BOOL)isEqual:(id)object {
    if (![JSString isString:object]) return NO;
    JSString *string = (JSString *)object;
    return _string ? [_string isEqualToString:[string value]] : [_mstring isEqualToString:[string mutableValue]];
}

- (NSUInteger)hash {
    return _string ? [_string hash] : [_mstring hash];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (JSString *)sort:(NSInteger (*)(id, id, void *))sorter {
    NSMutableArray *arr = [NSMutableArray new];
    NSUInteger i = 0;
    NSUInteger len = [_string count];
    for (i = 0; i < len; i++) {
        [arr addObject:[_string substringWithRange:NSMakeRange(i, 1)]];
    }
    return [[JSString alloc] initWithString:[[arr sortedArrayUsingFunction:sorter context:nil] componentsJoinedByString:@""]];
}

- (JSString *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    NSMutableArray *arr = [NSMutableArray new];
    NSUInteger i = 0;
    NSUInteger len = [_string count];
    for (i = 0; i < len; i++) {
        [arr addObject:[[JSString alloc] initWithString:[_string substringWithRange:NSMakeRange(i, 1)]]];
    }
    return [self joined:[arr sortedArrayUsingComparator:comparator] with:@""];
}

/** Returns the result of concatenating the strings in the given array with the separator. */
- (JSString *)joined:(NSArray<JSString *> *)arr with:(NSString *)separator {
    NSMutableString *res = [NSMutableString new];
    NSUInteger i = 0;
    NSUInteger len = [arr count];
    for (i = 0; i < len; i++) {
        [res appendFormat:@"%@%@", [(JSString *)arr[i] value], separator];
    }
    return [[JSString alloc] initWithString:res];
}

#pragma mark - Mutable

- (void)append:(JSString *)string {
    if (!_isMutable) [[[JSError alloc] initWithFormat:IsImmutableError, [self dataTypeName]] throw];
    [_mstring appendString: [string isMutable] ? [string mutableValue] : [string value]];
}

- (void)appendString:(NSString *)string {
    if (!_isMutable) [[[JSError alloc] initWithFormat:IsImmutableError, [self dataTypeName]] throw];
    [_mstring appendString:string];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[JSString alloc] initWithString:_string];
    [elem setIsImported:_isImported];
    [elem setIsMutable:NO];
    [elem setMutableValue:_mstring];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    id elem = [[JSString alloc] initWithString:_mstring];
    [elem setIsImported:_isImported];
    [elem setIsMutable:YES];
    [elem setValue:_string];
    return elem;
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ isMutable: %hhd meta: %@>", NSStringFromClass([self class]),
            self, _isMutable ? _mstring : _string, _isMutable, _meta];
}

- (NSString *)description {
    return _isMutable ? _mstring : _string;
}

@end
