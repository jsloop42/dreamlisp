//
//  JSFunction.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSFunction.h"

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

+ (BOOL)isFunction:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSFunction *)dataToFunction:(JSData *)data {
    return [self dataToFunction:data position:-1];
}

+ (JSFunction *)dataToFunction:(JSData *)data position:(NSInteger)position {
    if (![self isFunction:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithArity, @"'function'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatch, @"'function'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSFunction *)data;
}

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

- (NSString *)dataTypeName {
    return @"function";
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

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - params: %@, ast: %@ meta: %@>", NSStringFromClass([self class]), self, [_params description], _ast, _meta];
}

@end
