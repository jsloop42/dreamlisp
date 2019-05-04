//
//  JSFunction.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSFunction.h"

@implementation JSFunction {
    id<JSDataProtocol> (^_fn)(NSMutableArray *);
    id<JSDataProtocol> _ast;
    NSMutableArray *_params;
    Env *_env;
    BOOL _isMacro;
    id<JSDataProtocol> _meta;
    NSInteger _argsCount;
    NSInteger _position;
}

@synthesize fn = _fn;
@synthesize argsCount = _argsCount;
@synthesize ast = _ast;
@synthesize params = _params;
@synthesize env = _env;
@synthesize macro = _isMacro;
@synthesize meta = _meta;
@synthesize value;

+ (BOOL)isFunction:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (JSFunction *)dataToFunction:(id<JSDataProtocol>)data {
    return [self dataToFunction:data position:-1];
}

+ (JSFunction *)dataToFunction:(id<JSDataProtocol>)data position:(NSInteger)position {
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

- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(id<JSDataProtocol> (^)(NSMutableArray *))fn {
    self = [super init];
    if (self) {
        _fn = fn;
        _ast = ast;
        _params = params;
        _env = env;
        _isMacro = isMacro;
        _meta = meta;
        _argsCount = [self isVariadic] ? -1 : [_params count];
    }
    return self;
}

- (instancetype)initWithFn:(id<JSDataProtocol> (^)(NSMutableArray *))fn {
    self = [super init];
    if (self) _fn = fn;
    return self;
}

- (instancetype)initWithFn:(id<JSDataProtocol> (^)(NSMutableArray *))fn argCount:(NSInteger)count {
    self = [super init];
    if (self) {
        _fn = fn;
        _argsCount = count;
    }
    return self;
}

- (instancetype)initWithMacro:(JSFunction *)func {
    self = [super init];
    if (self) self = [self initWithAst:func.ast params:func.params env:func.env macro:YES meta:func.meta fn:func.fn];
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta func:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:func.isMacro meta:meta fn:func.fn];
        [self setArgsCount:[func argsCount]];
    }
    return self;
}

- (instancetype)initWithFunction:(JSFunction *)function {
    self = [super init];
    if (self)
        self = [self initWithAst:[function ast] params:[function params] env:[function env] macro:[function isMacro] meta:[function meta] fn:[function fn]];
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"function";
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (id<JSDataProtocol>)apply {
    return _fn([NSMutableArray new]);
}

- (id<JSDataProtocol>)apply:(NSMutableArray *)args {
    return _fn(args);
}

- (BOOL)isVariadic {
    return (_argsCount == -1) || ([_params count] >= 2 && [JSSymbol isSymbol:[_params nth:[_params count] - 2] withName:@"&"]);
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[JSFunction alloc] initWithFunction:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSFunction alloc] initWithFunction:self];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - params: %@, ast: %@ meta: %@>", NSStringFromClass([self class]), self, [_params description], _ast, _meta];
}

@end
