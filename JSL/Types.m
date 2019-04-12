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
@end

#pragma mark TypeUtils

@interface TypeUtils: NSObject
+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block;
@end

@implementation TypeUtils

+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block {
    NSMutableArray *acc = [NSMutableArray new];
    [array enumerateObjectsUsingBlock:^(id arg, NSUInteger idx, BOOL *stop) {
        [acc addObject:block(arg)];
    }];
    return acc;
}
@end

#pragma mark String

@implementation JSString {
    NSString *_string;
}

@synthesize value = _string;

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

- (NSString *)dataType {
    return [self className];
}

- (NSString *)value {
    return _string;
}

- (void)setValue:(NSString *)string {
    _string = string;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[self class] new];
    if (copy) {
        [copy setValue:_string];
    }
    return copy;
}

@end

#pragma mark NSString

@implementation NSString (JSDataProtocol)

-(NSString *)dataType {
    return @"NSString";
}

@end

#pragma mark Keyword

@implementation JSKeyword {
    NSString *_string;
}

@synthesize value = _string;

- (instancetype)init {
    self = [super init];
    if (self) {
        self = [self initWithString:@""];
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

- (instancetype)initWithKeyword:(NSString *)string {
    self = [super init];
    if (self) {
        _string = [string stringByReplacingCharactersInRange:NSMakeRange(0, 0) withString:[self keyword]];
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)keyword {
    return @":";
}

- (NSString *)value {
    return _string;
}

-(void)setValue:(NSString *)value {
    _string = value;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[self class] new];
    if (copy) {
        [copy setValue:_string];
    }
    return copy;
}

@end

#pragma mark HashMap

@implementation JSHashMap {
    NSMutableDictionary *dict;
}

@synthesize meta;

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

- (instancetype)initWithArray:(NSMutableArray *)array {
    self = [super init];
    if (self) {
        dict = [self fromArray:array];
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
        info(@"JSError: Odd number of elements in the array.");
        return dict;
    }
    for (i = 0; i < len; i = i + 2) {
        [_dict setObject:array[i + 1] forKey:array[i]];
    }
    return _dict;
}

- (JSData *)objectForString:(NSString *)key {
    return [dict objectForKey:key];
}

- (JSData *)objectForKey:(JSString *)key {
    return [dict objectForKey:key];
}

- (void)setValue:(JSData *)value forKey:(NSString *)key {
    [dict setValue:value forKey:key];
}

- (NSUInteger)count {
    return [dict count];
}

- (NSMutableDictionary *)value {
    return dict;
}

- (NSArray *)allKeys {
    return [dict allKeys];
}

- (NSArray *)allValues {
    return [dict allValues];
}

@end

#pragma mark List

@implementation JSList {
    NSMutableArray *array;
}

@synthesize meta;

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

- (NSString *)dataType {
    return [self className];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:array withBlock:block];
}

@end

#pragma mark NSArray

@implementation NSArray (JSDataProtocol)

-(NSString *)dataType {
    return @"NSArray";
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:[self mutableCopy] withBlock:block];
}

@end

#pragma mark Number

@implementation JSNumber {
    NSDecimalNumber *n;
    NSString *decimalPattern;
}

@synthesize meta;

- (instancetype)initWithDouble:(double)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithDouble:number];
    }
    return self;
}

- (instancetype)initWithInt:(int)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithInt:number];
    }
    return self;
}

- (instancetype)initWithString:(NSString *)string {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = [[NSDecimalNumber alloc] initWithString:string];
    }
    return self;
}

- (instancetype)initWithNumber:(NSDecimalNumber *)number {
    self = [super init];
    if (self) {
        [self bootstrap];
        n = number;
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
    return [n isEqualToNumber:[number val]];
}

- (double)doubleValue {
    return [n doubleValue];
}

- (int)intValue {
    return [n intValue];
}

- (NSDecimalNumber *)val {
    return n;
}

- (BOOL)isDouble {
    if ([Utils matchString:[self string] withPattern:decimalPattern]) {
        return YES;
    }
    return NO;
}

- (NSString *)string {
    return [n stringValue];
}

@end

#pragma mark NSNumber

@implementation NSNumber (JSDataProtocol)

-(NSString *)dataType {
    return @"NSNumber";
}

@end

#pragma mark NSDecimalNumber

@implementation NSDecimalNumber (JSDataProtocol)

-(NSString *)dataType {
    return @"NSDecimalNumber";
}

@end

#pragma mark Symbol

@implementation JSSymbol {
    NSString *_name;
}

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
    }
    return self;
}

-(NSString *)dataType {
    return [self className];
}

- (NSString *)name {
    return _name;
}

- (BOOL)isEqual:(JSSymbol *)symbol {
    return [_name isEqualToString:[symbol name]];
}

@end

#pragma mark Atom

@implementation JSAtom {
    JSData *_data;
}

- (instancetype)initWithData:(JSData *)data {
    self = [super init];
    if (self) {
        _data = data;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (JSData *)value {
    return _data;
}

@end

#pragma mark Nil

@implementation JSNil {
    NSString *_dataType;
}

@synthesize dataType = _dataType;

- (instancetype)init {
    self = [super init];
    if (self) {
        _dataType = [self className];
    }
    return self;
}

@end

#pragma mark Bool

@implementation JSBool {
    BOOL _flag;
}

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
        _flag = [object val];
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

-(BOOL)val {
    return _flag;
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

- (NSString *)dataType {
    return [self className];
}

- (JSData *)apply {
    return _fn([NSMutableArray new]);
}

- (JSData *)apply:(NSMutableArray*)args {
    return _fn(args);
}

@end
