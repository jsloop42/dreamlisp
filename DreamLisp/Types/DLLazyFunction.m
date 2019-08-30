//
//  DLLazyFunction.m
//  DreamLisp
//
//  Created by jsloop on 30/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLLazyFunction.h"

@implementation DLLazyFunction {
    void (^_fn)(DLLazySequence *seq, NSMutableArray *xs);
    id<DLDataProtocol> _ast;
    NSMutableArray *_params;
    Env *_env;
    BOOL _isMacro;
    id<DLDataProtocol> _meta;
    NSInteger _argsCount;
    NSInteger _position;
    /** The name with arity associated with the function. */
    NSString *_name;
    NSString *_moduleName;
    BOOL _isImported;
    BOOL _isMutable;
}

@synthesize fn = _fn;
@synthesize argsCount = _argsCount;
@synthesize ast = _ast;
@synthesize params = _params;
@synthesize env = _env;
@synthesize macro = _isMacro;
@synthesize meta = _meta;
@synthesize name = _name;
@synthesize value;
@synthesize moduleName = _moduleName;
@synthesize isImported = _isImported;
@synthesize isMutable = _isMutable;

+ (BOOL)isLazyFunction:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (DLLazyFunction *)dataToLazyFunction:(id<DLDataProtocol>)data {
    return [self dataToLazyFunction:data position:-1];
}

+ (DLLazyFunction *)dataToLazyFunction:(id<DLDataProtocol>)data position:(NSInteger)position {
    if (![self isLazyFunction:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithArity, @"'lazy-function'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatch, @"'lazy-function'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLLazyFunction *)data;
}

+ (DLLazyFunction *)dataToLazyFunction:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLLazyFunction isLazyFunction:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'lazy-function'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'lazy-function'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLLazyFunction *)data;
}

- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn name:(NSString *)name {
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

- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn {
    self = [super init];
    if (self) {
        self = [self initWithAst:ast params:params env:env macro:isMacro meta:meta fn:fn name:@""];
        _moduleName = [env moduleName];
    }
    return self;
}

- (instancetype)initWithFn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _name = name;
    }
    return self;
}

- (instancetype)initWithFn:(void (^)(DLLazySequence *seq, NSMutableArray *xs))fn argCount:(NSInteger)count name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _argsCount = count;
        _name = name;
    }
    return self;
}

- (instancetype)initWithMacro:(DLLazyFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:YES meta:func.meta fn:func.fn name:func.name];
        _moduleName = [func.env moduleName];
    }
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta func:(DLLazyFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:func.isMacro meta:meta fn:func.fn name:func.name];
        [self setArgsCount:[func argsCount]];
        _moduleName = [func.env moduleName];
    }
    return self;
}

- (instancetype)initWithFunction:(DLLazyFunction *)function {
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

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (void)apply:(DLLazySequence *)seq {
    _fn(seq, [NSMutableArray new]);
}

- (void)apply:(NSMutableArray *)args forLazySequence:(DLLazySequence *)seq {
    _fn(seq, args);
}

- (BOOL)isVariadic {
    return (_argsCount == -1) || ([_params count] >= 2 && [DLSymbol isSymbol:[_params nth:[_params count] - 2] withName:@"&"]);
}

- (NSString *)name {
    return [_name isEmpty]
            ? [[NSString alloc] initWithFormat:@"#<fn/%@>", _argsCount == -1 ? @"n" : [[NSString alloc] initWithFormat:@"%ld", _argsCount]]
            : _name;
}

- (NSString *)value {
    return _name;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (BOOL)isEqual:(id)object {
    if (![DLFunction isFunction:object]) return NO;
    DLFunction *fn = (DLFunction *)object;
    return [_name isEqual:[fn name]] && _argsCount == [fn argsCount] && [_ast isEqual:[fn ast]] && _isMacro == [fn isMacro];
}

- (NSUInteger)hash {
    return [_name hash] + [_params hash] + _isMacro ? 1 : 0;
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[DLLazyFunction alloc] initWithFunction:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [self name];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"<%@ %p - params: %@, ast: %@ meta: %@>", NSStringFromClass([self class]), self, [_params description], _ast, _meta];
}

@end
