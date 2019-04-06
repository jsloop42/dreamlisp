//
//  Types.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Types.h"

#pragma mark String

@implementation JSData
@synthesize dataType;
@end

@implementation JSString {
    NSString *string;
    BOOL _isKeyword;
    NSString *_dataType;
}

@synthesize dataType = _dataType;
@synthesize isKeyword = _isKeyword;

- (instancetype)init {
    self = [super init];
    if (self) {
        string = @"";
        _isKeyword = NO;
        _dataType = [self className];
    }
    return self;
}

- (instancetype)initWithString:(NSString *)str {
    self = [super init];
    if (self) {
        string = str;
        _dataType = [self className];
    }
    return self;
}

- (instancetype)initWithKeyword:(NSString *)str {
    self = [super init];
    if (self) {
        string = str;
        _isKeyword = YES;
        _dataType = @"JSKeyword";
    }
    return self;
}

- (NSString *)value {
    return string;
}

@end

#pragma mark HashMap

@implementation JSHashMap {
    NSMutableDictionary *dict;
    NSString *_dataType;
}

@synthesize dataType = _dataType;
@synthesize meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        dict = [NSMutableDictionary new];
        _dataType = [self className];
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

- (NSMutableDictionary *)fromArray:(NSMutableArray *)array {
    NSMutableDictionary* dict;
    NSUInteger i = 0, len = [array count];
    if (len % 2 != 0) {
        info(@"JSError: Odd number of elements in the array.");
        return dict;
    }
    for (i = 0; i < len; i = i + 2) {
        [dict setObject:array[i + 1] forKey:array[i]];
    }
    return dict;
}

- (JSData*)valueForKey:(NSString*)key {
    return [dict valueForKey:key];
}

- (void)setValue:(JSData*)value forKey:(NSString *)key {
    [dict setValue:value forKey:key];
}

- (NSUInteger)count {
    return [dict count];
}

- (NSMutableDictionary *)value {
    return dict;
}

@end

#pragma mark List

@implementation JSList {
    NSMutableArray *array;
    NSString *_dataType;
}

@synthesize dataType = _dataType;
@synthesize meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        array = [NSMutableArray new];
        _dataType = [self className];
    }
    return self;
}

- (instancetype)initWith:(NSArray *)list {
    self = [super init];
    if (self) {
        array = [[NSMutableArray alloc] initWithArray:list];
        _dataType = [self className];
    }
    return self;
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
    NSArray *arr = [array objectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(1, [array count] - 1)]];
    return [[JSList alloc] initWith:arr];
}

- (JSData *)last {
    return [array objectAtIndex:[array count] - 1];
}

@end

#pragma mark Vector

@implementation JSVector {
    NSMutableArray *array;
    NSString *_dataType;
    JSData *_meta;
}

@synthesize dataType = _dataType;
@synthesize meta = _meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        array = [NSMutableArray new];
        _dataType = [self className];
    }
    return self;
}

- (instancetype)initWith:(NSArray *)list {
    self = [super init];
    if (self) {
        array = [[NSMutableArray alloc] initWithArray:list];
        _dataType = [self className];
    }
    return self;
}

@end

#pragma mark Number

@implementation JSNumber {
    NSNumber *n;
    NSString *_dataType;
}

@synthesize dataType = _dataType;
@synthesize meta;

- (instancetype)initWith:(float)num {
    self = [super init];
    if (self) {
        n = [[NSNumber alloc] initWithFloat:num];
        _dataType = [self className];
    }
    return self;
}

- (BOOL)isEqual:(JSNumber *)num {
    return [n isEqualToNumber:[num val]];
}

- (float)value {
    return [n floatValue];
}

- (NSNumber *)val {
    return n;
}

@end

#pragma mark Symbol

@implementation JSSymbol {
    NSString *_name;
    NSString *_dataType;
}

@synthesize dataType = _dataType;

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
        _dataType = [self className];
    }
    return self;
}

- (NSString *)name {
    return _name;
}

- (BOOL)isEqual:(JSSymbol *)sym {
    return [_name isEqualToString:[sym name]];
}

@end

#pragma mark Atom

@implementation JSAtom {
    NSString *_dataType;
    JSData *_data;
}

@synthesize dataType = _dataType;

- (instancetype)initWith:(JSData *) data {
    self = [super init];
    if (self) {
        _data = data;
        _dataType = [self className];
    }
    return self;
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

#pragma mark True

@implementation JSTrue {
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

#pragma mark False

@implementation JSFalse {
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

@implementation JSFunction {
    NSString *_dataType;
    JSData *(^_fn)(NSMutableArray *);
    JSData *_ast;
    NSMutableArray *_params;
    Env *_env;
    BOOL _isMacro;
    JSData *_meta;
}

@synthesize dataType = _dataType;
@synthesize fn = _fn;
@synthesize ast = _ast;
@synthesize params = _params;
@synthesize env = _env;
@synthesize isMacro = _isMacro;
@synthesize meta = _meta;

- (instancetype)initWithAst:(JSData *)ast params:(NSMutableArray *)params env:(Env *)env isMacro:(BOOL)isMacro meta:(JSData *)meta
                         fn:(JSData *(^)(NSMutableArray *))fn {
    self = [super init];
    if (self) {
        _dataType = [self className];
        _fn = fn;
        _ast = ast;
        _params = params;
        _env = env;
        _isMacro = isMacro;
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithIsMacro:(BOOL)isMacro func:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env isMacro:isMacro meta:func.meta fn:func.fn];
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta func:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env isMacro:func.isMacro meta:meta fn:func.fn];
    }
    return self;
}

- (JSData *)apply {
    return _fn([NSMutableArray new]);
}

- (JSData *)apply:(NSMutableArray*)args {
    return _fn(args);
}

@end
