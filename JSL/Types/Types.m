//
//  Types.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Types.h"

@implementation JSData

@synthesize dataType;
@synthesize value;
@synthesize meta;

- (BOOL)hasMeta {
    return NO;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [JSData new];
    return copy;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    id copy = [JSData new];
    return copy;
}

@end

#pragma mark String

@implementation JSString {
    NSString *_string;
    JSData *_meta;
}

@synthesize value = _string;
@synthesize meta = _meta;

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
        if (!_string && err) {
            @throw [[NSException alloc] initWithName:JSL_FILE_READ_ERROR reason:JSL_FILE_READ_ERROR_MSG userInfo:nil];
        }
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

- (instancetype)initWithMeta:(JSData *)meta string:(JSString *)string {
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

- (NSString *)value {
    return _string;
}

- (void)setValue:(NSString *)string {
    _string = string;
}

- (BOOL)isEqual:(JSString *)string {
    return [_string isEqualToString:[string value]];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSString alloc] initWithString:_string];
    return copy;
}

@end

#pragma mark Keyword

@implementation JSKeyword {
    NSString *_string;
    JSData *_meta;
}

@synthesize value = _string;
@synthesize meta = _meta;

+ (BOOL)isKeyword:(NSString *)string {
    if ([[string substringToIndex:1] isEqual:@"\u029e"]) {
        return YES;
    }
    return NO;
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

- (instancetype)initWithMeta:(JSData *)meta keyword:(JSKeyword *)keyword {
    self = [super init];
    if (self) {
        _string = [keyword value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)value {
    return _string;
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

- (void)setValue:(NSString *)value {
    _string = value;
}

- (BOOL)isEqual:(JSKeyword *)keyword {
    return [_string isEqualToString:[keyword value]];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSKeyword alloc] initWithKeyword:[self string]];
    return copy;
}

@end

#pragma mark Symbol

@implementation JSSymbol {
    NSString *_name;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta symbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        _name = [symbol name];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)name {
    return _name;
}

- (BOOL)isEqual:(JSSymbol *)symbol {
    return [_name isEqualToString:[symbol name]];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSSymbol alloc] initWithName:_name];
    return copy;
}

@end

#pragma mark HashMap

@implementation JSHashMap {
    NSMutableDictionary *dict;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        dict = [NSMutableDictionary new];
    }
    return self;
}

- (instancetype)initWithDictionary:(NSMutableDictionary *)dictionary {
    self = [super init];
    if (self) {
        dict = dictionary;
    }
    return self;
}

- (instancetype)initWithStringKey:(NSMutableDictionary<NSString *, id> *)dictionary {
    self = [super init];
    if (self) {
        dict = dictionary;
    }
    return self;
}

- (instancetype)initWithArray:(NSMutableArray *)array {
    self = [super init];
    if (self) {
        dict = [self fromArray:array];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta hashmap:(JSHashMap *)hashmap {
    self = [super init];
    if (self) {
        dict = [hashmap value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSMutableDictionary *)fromArray:(NSMutableArray *)array {
    NSMutableDictionary* _dict = [NSMutableDictionary new];
    NSUInteger i = 0, len = [array count];
    if (len % 2 != 0) {
        error(@"JSError: Odd number of elements in the array.");
        return dict;
    }
    for (i = 0; i < len; i = i + 2) {
        NSString *key = nil;
        JSData *dkey = (JSData *)[array[i] dataType];
        if (dkey == nil) @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        if (dkey != nil) {
            if ([dkey isEqual:@"JSSymbol"]) {
                key = [(JSSymbol *)array[i] name];
            } else if ([dkey isEqual:@"JSString"]) {
                key = [(JSString *)array[i] value];
            } else if ([dkey isEqual:@"JSKeyword"]) {
                key = [(JSKeyword *)array[i] encoded];
            } else if ([dkey isEqual:@"NSString"]) {
                key = array[i];
            }
        }
        [_dict setObject:(JSData *)array[i + 1] forKey:key];
    }
    return _dict;
}

- (JSData *)objectForKey:(NSString *)key {
    return [dict objectForKey:key];
}

- (void)setObject:(JSData *)object forKey:(NSString *)key {
    [dict setObject:object forKey:key];
}

- (NSUInteger)count {
    return [dict count];
}

- (NSMutableDictionary *)value {
    return dict;
}

- (void)setValue:(NSMutableDictionary *)hm {
    dict = hm;
}

- (NSArray *)allKeys {
    return [dict allKeys];
}

- (NSArray *)allValues {
    return [dict allValues];
}

- (BOOL)isEqual:(JSHashMap *)hashmap {
    if ([dict count] != [hashmap count]) {
        return NO;
    }
    NSString *key = nil;
    JSData *lval = nil;
    JSData *rval = nil;
    for (key in dict) {
        lval = [dict objectForKey:key];
        rval = [hashmap objectForKey:key];
        if (!lval || !rval || [lval isNotEqualTo:rval]) {
            return NO;
        }
    }
    return YES;
}

- (JSData *)addEntriesFromDictionary:(NSMutableDictionary *)xs {
    JSHashMap *hm = [[JSHashMap alloc] initWithDictionary:dict];
    NSMutableDictionary *hmDict = [hm mutableCopy];
    [hmDict addEntriesFromDictionary:xs];
    return hm;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSHashMap alloc] initWithDictionary:dict];
    return copy;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    id copy = [[JSHashMap allocWithZone:zone] init];
    [(JSHashMap *)copy setValue:[dict mutableCopyWithZone:zone]];
    return copy;
}

@end

#pragma mark List

@implementation JSList {
    NSMutableArray *array;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        array = [NSMutableArray new];
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)list {
    self = [super init];
    if (self) {
        array = [[NSMutableArray alloc] initWithArray:list];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta list:(JSList *)list {
    self = [super init];
    if (self) {
        array = [list value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (void)add:(JSData *)object {
    [array addObject:object];
}

- (void)add:(JSData *)object atIndex:(NSUInteger)index {
    [array insertObject:object atIndex:index];
}

- (void)remove:(JSData *)object {
    [array removeObject:object];
}

- (void)removeAtIndex:(NSUInteger)index {
    [array removeObjectAtIndex:index];
}

- (void)setValue:(NSMutableArray *)aArray {
    array = aArray;
}

- (NSMutableArray *)value {
    return array;
}

- (NSUInteger)count {
    return [array count];
}

- (JSData *)first {
    return [array firstObject];
}

- (JSData *)second {
    return [array objectAtIndex:1];
}

- (JSData *)rest {
    NSMutableArray *arr = [array mutableCopy];
    [arr removeObjectAtIndex:0];
    return [[JSList alloc] initWithArray:arr];
}

- (JSData *)last {
    return [array objectAtIndex:[array count] - 1];
}

- (JSData *)dropLast {
    NSMutableArray *arr = [array mutableCopy];
    [arr removeLastObject];
    return [[JSList alloc] initWithArray:arr];
}

- (JSData *)nth:(NSInteger)n {
    return [array objectAtIndex:n];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:array withBlock:block];
}

- (BOOL)isEmpty {
    return [array count] == 0;
}

- (BOOL)isEqual:(JSList *)list {
    NSUInteger len = [array count];
    NSUInteger i = 0;
    if (len != [list count]) {
        return NO;
    }
    for (i = 0; i < len; i++) {
        if (![array[i] isEqual:[list nth:i]]) {
            return NO;
        }
    }
    return YES;
}

/** Returns a new list which the reverse of the current list. */
- (JSList *)reverse {
    return [[JSList alloc] initWithArray:[[[[array rest] reverseObjectEnumerator] allObjects] mutableCopy]];
}

/** Drops n elements. */
- (JSList *)drop:(NSInteger)n {
    NSMutableArray *arr = [array mutableCopy];
    if (n > 0 && n <= [arr count]) {
        [arr removeObjectsInRange:NSMakeRange(0, n)];
        return [[JSList alloc] initWithArray:arr];
    }
    return nil;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSList alloc] initWithArray:array];
    return copy;
}

@end

#pragma mark Vector

@implementation JSVector {
    NSMutableArray *array;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        array = [NSMutableArray new];
        [super setValue:array];
    }
    return self;
}

- (instancetype)initWithArray:(NSArray *)list {
    self = [super init];
    if (self) {
        array = [[NSMutableArray alloc] initWithArray:list];
        [super setValue:array];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta vector:(JSVector *)vector {
    self = [super init];
    if (self) {
        array = [vector value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:array withBlock:block];
}

- (BOOL)isEqual:(JSVector *)vector {
    NSUInteger len = [array count];
    NSUInteger i = 0;
    if (len != [vector count]) {
        return NO;
    }
    for (i = 0; i < len; i++) {
        if (![array[i] isEqual:[vector nth:i]]) {
            return NO;
        }
    }
    return YES;
}

- (JSList *)list {
    return [[JSList alloc] initWithArray:array];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSVector alloc] initWithArray:array];
    return copy;
}

@end

#pragma mark Number

@implementation JSNumber {
    NSDecimalNumber *n;
    NSString *decimalPattern;
    BOOL _isDouble;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)initWithDouble:(double)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithDouble:number];
        _isDouble = YES;
    }
    return self;
}

- (instancetype)initWithInt:(int)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithInt:number];
        _isDouble = NO;
    }
    return self;
}

- (instancetype)initWithInteger:(NSUInteger)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithInteger:number];
        _isDouble = NO;
    }
    return self;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithString:string];
        _isDouble = [self checkDouble:[n stringValue]];
    }
    return self;
}

- (instancetype)initWithNumber:(NSDecimalNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = number;
        _isDouble = [self checkDouble:[n stringValue]];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta number:(JSNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [number value];
        _meta = meta;
        _isDouble = [self checkDouble:[n stringValue]];
    }
    return self;
}


- (void)bootstrap {
    decimalPattern = @"\\d+(\\.\\d+)";
}

- (NSString *)dataType {
    return [self className];
}

- (BOOL)isEqual:(JSNumber *)number {
    return [n isEqualToNumber:[number value]];
}

- (double)doubleValue {
    return [n doubleValue];
}

- (int)intValue {
    return [n intValue];
}

- (NSUInteger)integerValue {
    return [n integerValue];
}

- (NSDecimalNumber *)value {
    return n;
}

- (BOOL)checkDouble:(NSString *)string {
    if ([Utils matchString:string withPattern:decimalPattern]) {
        return YES;
    }
    return NO;
}

- (BOOL)isDouble {
    return _isDouble;
}

- (NSString *)string {
    if (_isDouble && ![self checkDouble:[n stringValue]]) {
        return [NSString stringWithFormat:@"%.01f", [n doubleValue]];
    }
    return [n stringValue];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSNumber alloc] initWithNumber:n];
    return copy;
}

@end

#pragma mark Atom

@implementation JSAtom {
    JSData *_data;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)initWithData:(JSData *)data {
    self = [super init];
    if (self) {
        _data = data;
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta atom:(JSAtom *)atom {
    self = [super init];
    if (self) {
        _data = [atom value];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (JSData *)value {
    return _data;
}

- (void)setValue:(JSData *)data {
    _data = data;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSAtom alloc] initWithData:_data];
    return copy;
}

@end

#pragma mark Nil

@implementation JSNil {
    NSString *_dataType;
    JSData *_meta;
}

@synthesize dataType = _dataType;
@synthesize meta = _meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        _dataType = [self className];
    }
    return self;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

@end

#pragma mark Bool

@implementation JSBool {
    BOOL _flag;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)initWithBool:(BOOL)flag {
    self = [super init];
    if (self) {
        _flag = flag;
    }
    return self;
}

- (instancetype)initWithJSBool:(JSBool *)object {
    self = [super init];
    if (self) {
        _flag = [object value];
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (BOOL)isEqual:(JSBool *)boolean {
    return _flag == [boolean value];
}

- (BOOL)value {
    return _flag;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

@end

#pragma mark Function

@implementation JSFunction {
    JSData *(^_fn)(NSMutableArray *);
    JSData *_ast;
    NSMutableArray *_params;
    Env *_env;
    BOOL _isMacro;
    JSData *_meta;
}

@synthesize fn = _fn;
@synthesize ast = _ast;
@synthesize params = _params;
@synthesize env = _env;
@synthesize macro = _isMacro;
@synthesize meta = _meta;

- (instancetype)initWithAst:(JSData *)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(JSData * _Nullable)meta
                         fn:(JSData *(^)(NSMutableArray *))fn {
    self = [super init];
    if (self) {
        _fn = fn;
        _ast = ast;
        _params = params;
        _env = env;
        _isMacro = isMacro;
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithFn:(JSData * (^)(NSMutableArray *))fn {
    self = [super init];
    if (self) {
        _fn = fn;
    }
    return self;
}

- (instancetype)initWithMacro:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:YES meta:func.meta fn:func.fn];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta func:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:func.isMacro meta:meta fn:func.fn];
    }
    return self;
}

- (instancetype)initWithFunction:(JSFunction *)function {
    self = [super init];
    if (self) {
        self = [self initWithAst:[function ast] params:[function params] env:[function env] macro:[function isMacro] meta:[function meta] fn:[function fn]];
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (JSData *)apply {
    return _fn([NSMutableArray new]);
}

- (JSData *)apply:(NSMutableArray *)args {
    return _fn(args);
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSFunction alloc] initWithFunction:self];
    return copy;
}

@end
