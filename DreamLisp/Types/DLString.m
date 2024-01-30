//
//  DLString.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLString.h"

@implementation DLString {
    NSString *_string;
    NSMutableString *_mstring;
    id<DLDataProtocol> _meta;
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
    return [[(DLString *)object value] isEqual:name];
}

+ (DLString *)dataToString:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToString:data position:-1 fnName:fnName];
}

+ (DLString *)dataToString:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLString isString:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'string'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'string'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLString *)data;
}

+ (DLString *)mutable {
    return [[DLString alloc] initWithMutableString];
}

+ (DLString *)stringWithString:(NSString *)string {
    return [[DLString alloc] initWithString:string];
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

- (instancetype)initWithArray:(NSMutableArray<DLString *> *)array {
    self = [super init];
    if (self) {
        [self bootstrap];
        _isMutable = YES;
        _mstring = [NSMutableString new];
        DLString *str = nil;
        for (str in array) {
            [_mstring appendString:[str value]];
        }
    }
    return self;
}

- (instancetype)initWithContentsOfFile:(NSString *)filePath {
    self = [super init];
    if (self) {
        [self bootstrap];
        NSError *err = nil;
        _string = [[NSString alloc] initWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&err];
        if (!_string && err) [[[DLError alloc] initWithUserInfo:[err userInfo]] throw];
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

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta string:(DLString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        _string = [string value];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _string = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_value"];
        _mstring = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_mstring"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLString_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_string forKey:@"DLString_value"];
    [coder encodeObject:_mstring forKey:@"DLString_mstring"];
    [coder encodeObject:_meta forKey:@"DLString_meta"];
    [coder encodeObject:_moduleName forKey:@"DLString_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLString_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLString_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLString_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
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

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (BOOL)isEmpty {
    return [_string count] == 0;
}

- (NSUInteger)count {
    return _string ? [_string length] : [_mstring length];
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
    return [self substringFrom:start count:end];
}

- (NSString *)substringFrom:(NSInteger)start to:(NSInteger)end {
    NSString *str = [self value];
    if (!str) return @"";
    NSUInteger len = [str count];
    if (start < 0) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, start, 0] throw];
    if (end >= len) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, end, len] throw];
    if (end < start) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, end, start] throw];
    return [str substringWithRange:NSMakeRange(start, end - start + 1)];
}

- (NSString *)substringFrom:(NSInteger)start count:(NSInteger)count {
    NSString *str = [self value];
    if (!str) return @"";
    NSUInteger len = [str count];
    if (start < 0) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, start, 0] throw];
    if (count > len) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, count, count < 0 ? 0 : len] throw];
    return [str substringWithRange:NSMakeRange(start, count)];
}

- (NSString * _Nullable)reverse {
    NSString *str = [self value];
    NSUInteger len = [str length];
    if (len <= 1) return str;
    NSStringEncoding encoding = NSHostByteOrder() == NS_BigEndian ? NSUTF32BigEndianStringEncoding : NSUTF32LittleEndianStringEncoding;
    NSUInteger bytesCount = [str lengthOfBytesUsingEncoding:encoding];
    uint32_t *chars = malloc(bytesCount);
    if (!chars) return nil;
    [str getBytes:chars maxLength:bytesCount usedLength:nil encoding:encoding options:0 range:NSMakeRange(0, len) remainingRange:nil];
    NSUInteger charLen = bytesCount / sizeof(uint32_t);
    NSUInteger halfwayPoint = charLen / 2;
    NSUInteger i = 0;
    uint32_t c = 0;
    for (i = 0; i < halfwayPoint; ++i) {
        c = chars[charLen - i - 1];
        chars[charLen - i - 1] = chars[i];
        chars[i] = c;
    }
    return [[NSString alloc] initWithBytesNoCopy:chars length:bytesCount encoding:encoding freeWhenDone:YES];
}

- (void)setValue:(id)value {
    _string = value;
}

- (void)setMutableValue:(NSMutableString *)mutableValue {
    _mstring = mutableValue;
}

- (BOOL)isEqual:(id)object {
    if (![DLString isString:object]) return NO;
    DLString *string = (DLString *)object;
    return [[self value] isEqual:[string value]];
}

- (NSUInteger)hash {
    return _string ? [_string hash] : [_mstring hash];
}

- (NSInteger)sortValue {
    return [self hash];
}

- (DLString *)sort:(NSInteger (*)(id, id, void *))sorter {
    NSMutableArray *arr = [NSMutableArray new];
    NSUInteger i = 0;
    NSUInteger len = [_string count];
    for (i = 0; i < len; i++) {
        @autoreleasepool {
            [arr addObject:[_string substringWithRange:NSMakeRange(i, 1)]];
        }
    }
    return [[DLString alloc] initWithString:[[arr sortedArrayUsingFunction:sorter context:nil] componentsJoinedByString:@""]];
}

- (DLString *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator {
    NSMutableArray *arr = [NSMutableArray new];
    NSUInteger i = 0;
    NSUInteger len = [_string count];
    for (i = 0; i < len; i++) {
        @autoreleasepool {
            [arr addObject:[[DLString alloc] initWithString:[_string substringWithRange:NSMakeRange(i, 1)]]];
        }
    }
    return [self joined:[arr sortedArrayUsingComparator:comparator] with:@""];
}

/** Returns the result of concatenating the strings in the given array with the separator. */
- (DLString *)joined:(NSArray<DLString *> *)arr with:(NSString *)separator {
    return [[DLString alloc] initWithString:[arr componentsJoinedByString:separator]];
}

#pragma mark - Mutable

- (void)append:(DLString *)string {
    if (!_isMutable) [[[DLError alloc] initWithFormat:DLIsImmutableError, [self dataTypeName]] throw];
    [_mstring appendString: [string isMutable] ? [string mutableValue] : [string value]];
}

- (void)appendString:(NSString *)string {
    if (!_isMutable) [[[DLError alloc] initWithFormat:DLIsImmutableError, [self dataTypeName]] throw];
    [_mstring appendString:string];
}

- (void)append:(id)object atIndex:(NSInteger)index {
    if (!_isMutable) [[[DLError alloc] initWithFormat:DLIsImmutableError, [self dataTypeName]] throw];
    [_mstring insertString:[object description] atIndex:index];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id elem = [[DLString alloc] initWithString:_string];
    [elem setIsImported:_isImported];
    [elem setIsMutable:NO];
    [elem setMutableValue:_mstring];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    id elem = [[DLString alloc] initWithString:_mstring];
    [elem setIsImported:_isImported];
    [elem setIsMutable:YES];
    [elem setValue:_string];
    return elem;
}

- (NSString *)description {
    return _isMutable ? _mstring : _string;
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - value: %@ isMutable: %hhd meta: %@>", NSStringFromClass([self class]),
            self, _isMutable ? _mstring : _string, _isMutable, _meta];
}

@end
