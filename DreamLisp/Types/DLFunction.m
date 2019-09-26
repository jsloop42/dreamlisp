//
//  DLFunction.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLFunction.h"

@implementation DLFunction {
    id<DLDataProtocol> (^_fn)(NSMutableArray *);
    id<DLDataProtocol> _ast;
    NSMutableArray *_params;
    DLEnv *_env;
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

+ (BOOL)isFunction:(id)object {
    return [[object class] isEqual:[self class]];
}

+ (DLFunction *)dataToFunction:(id<DLDataProtocol>)data {
    return [self dataToFunction:data position:-1];
}

+ (DLFunction *)dataToFunction:(id<DLDataProtocol>)data position:(NSInteger)position {
    if (![self isFunction:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithArity, @"'function'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatch, @"'function'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLFunction *)data;
}

+ (DLFunction *)dataToFunction:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLFunction isFunction:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'function'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'function'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLFunction *)data;
}

- (void)dealloc {
    [DLLog debug:@"DLFunction dealloc"];
}

- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(DLEnv *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(id<DLDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name {
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

- (instancetype)initWithAst:(id<DLDataProtocol>)ast params:(NSMutableArray *)params env:(DLEnv *)env macro:(BOOL)isMacro meta:(id<DLDataProtocol> _Nullable)meta
                         fn:(id<DLDataProtocol> (^)(NSMutableArray *))fn {
    self = [super init];
    if (self) {
        self = [self initWithAst:ast params:params env:env macro:isMacro meta:meta fn:fn name:@""];
        _moduleName = [env moduleName];
    }
    return self;
}

- (instancetype)initWithFn:(id<DLDataProtocol> (^)(NSMutableArray *))fn name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _name = name;
    }
    return self;
}

- (instancetype)initWithFn:(id<DLDataProtocol> (^)(NSMutableArray *))fn argCount:(NSInteger)count name:(NSString *)name {
    self = [super init];
    if (self) {
        _fn = fn;
        _argsCount = count;
        _name = name;
    }
    return self;
}

- (instancetype)initWithMacro:(DLFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:YES meta:func.meta fn:func.fn name:func.name];
        _moduleName = [func.env moduleName];
    }
    return self;
}

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta func:(DLFunction *)func {
    self = [super init];
    if (self) {
        self = [self initWithAst:func.ast params:func.params env:func.env macro:func.isMacro meta:meta fn:func.fn name:func.name];
        [self setArgsCount:[func argsCount]];
        _moduleName = [func.env moduleName];
    }
    return self;
}

- (instancetype)initWithFunction:(DLFunction *)function {
    self = [super init];
    if (self) {
        self = [self initWithAst:function.ast params:function.params env:function.env macro:function.isMacro meta:function.meta fn:function.fn
                            name:function.name];
        _moduleName = [function.env moduleName];
        _isImported = [function isImported];
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _fn = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_fn"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_position"] integerValue];
        NSValue *isMacroValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_isMacro"];
        [isMacroValue getValue:&_isMacro];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_isMutable"];
        [isMutableValue getValue:&_isMutable];
        _ast = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_ast"];
        _name = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_name"];
        _env = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_env"];
        _params = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLFunction_params"];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_fn forKey:@"DLFunction_fn"];
    [coder encodeObject:_meta forKey:@"DLFunction_meta"];
    [coder encodeObject:_moduleName forKey:@"DLFunction_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLFunction_position"];
    NSValue *isMacroValue = [[NSValue alloc] initWithBytes:&_isMacro objCType:@encode(BOOL)];
    [coder encodeObject:isMacroValue forKey:@"DLFunction_isMacro"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLFunction_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLFunction_isMutable"];
    [coder encodeObject:_ast forKey:@"DLFunction_ast"];
    [coder encodeObject:_name forKey:@"DLFunction_name"];
    [coder encodeObject:_env forKey:@"DLFunction_env"];
    [coder encodeObject:_params forKey:@"DLFunction_params"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (NSString *)dataType {
    return NSStringFromClass([self class]);
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

- (id<DLDataProtocol>)apply {
    return _fn([NSMutableArray new]);
}

- (id<DLDataProtocol>)apply:(NSMutableArray *)args {
    return _fn(args);
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
    return [_name hash] + [_params hash] + (_isMacro ? 1 : 0);
}

- (NSInteger)sortValue {
    return [self hash];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[DLFunction alloc] initWithFunction:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [self name];
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %p - params: %@, ast: %@ meta: %@>", NSStringFromClass([self class]), self, [_params description], _ast, _meta];
}

@end
