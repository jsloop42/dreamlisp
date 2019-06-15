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
    /** The name with arity associated with the function. */
    NSString *_name;
    NSString *_moduleName;
    BOOL _isImported;
}

@synthesize fn = _fn;
@synthesize argsCount = _argsCount;
@synthesize ast = _ast;
@synthesize params = _params;
@synthesize env = _env;
@synthesize macro = _isMacro;
@synthesize meta = _meta;
@synthesize value;  // not used
@synthesize name = _name;
@synthesize moduleName = _moduleName;
@synthesize isImported = _isImported;

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

+ (JSFunction *)dataToFunction:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSFunction isFunction:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'function'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'function'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSFunction *)data;
}

- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(id<JSDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _ast = ast;
        _params = params;
        _env = env;
        _isMacro = isMacro;
        _meta = meta;
        _argsCount = [self isVariadic] ? -1 : [_params count];
        _name = name;
    }
    return self;
}

- (instancetype)initWithAst:(id<JSDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<JSDataProtocol> _Nullable)meta
                         fn:(id<JSDataProtocol> (^)(NSMutableArray *))fn {
    self = [super init];
    if (self) {
        self = [self initWithAst:ast params:params env:env macro:isMacro meta:meta fn:fn name:@""];
        _moduleName = [env moduleName];
    }
    return self;
}

- (instancetype)initWithFn:(id<JSDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _name = name;
    }
    return self;
}

- (instancetype)initWithFn:(id<JSDataProtocol> (^)(NSMutableArray *))fn argCount:(NSInteger)count name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _argsCount = count;
        _name = name;
    }
    return self;
}

- (instancetype)initWithMacro:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:YES meta:func.meta fn:func.fn name:func.name];
        _moduleName = [func.env moduleName];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta func:(JSFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:func.isMacro meta:meta fn:func.fn name:func.name];
        [self setArgsCount:[func argsCount]];
        _moduleName = [func.env moduleName];
    }
    return self;
}

- (instancetype)initWithFunction:(JSFunction *)function {
    self = [super init];
    if (self) {
        self = [self initWithAst:function.ast params:function.params env:function.env macro:function.isMacro meta:function.meta fn:function.fn
                            name:function.name];
        _moduleName = [function.env moduleName];
        _isImported = [function isImported];
    }
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

- (NSString *)name {
    return [_name isEmpty]
            ? [[NSString alloc] initWithFormat:@"#<fn/%@>", _argsCount == -1 ? @"n" : [[NSString alloc] initWithFormat:@"%ld", _argsCount]]
            : _name;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (BOOL)isEqual:(id)object {
    if (![JSFunction isFunction:object]) return NO;
    JSFunction *fn = (JSFunction *)object;
    return [_name isEqual:[fn name]] && _argsCount == [fn argsCount] && [_ast isEqual:[fn ast]] && _isMacro == [fn isMacro];
}

- (NSUInteger)hash {
    return [_name hash] + [_params hash] + _isMacro ? 1 : 0;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[JSFunction alloc] initWithFunction:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p - params: %@, ast: %@ meta: %@>", NSStringFromClass([self class]), self, [_params description], _ast, _meta];
}

@end
