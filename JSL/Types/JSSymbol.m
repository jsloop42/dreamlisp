//
//  JSSymbol.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSSymbol.h"

@implementation JSSymbol {
    NSString *_name;
    NSString *_initialName;
    id<JSDataProtocol> _meta;
    NSInteger _arity;
    NSInteger _initialArity;
    BOOL _isFunction;
    BOOL _hasNArity;
    NSInteger _position;
    /** Function name with arity if the symbol is bound to a function */
    NSString *_fnName;
    NSString *_moduleName;
}

@synthesize arity = _arity;
@synthesize initialArity = _initialArity;
@synthesize isFunction = _isFunction;
@synthesize hasNArity = _hasNArity;
@synthesize meta = _meta;
@synthesize value = _name;
@synthesize initialValue = _initialName;
@synthesize fnName = _fnName;

+ (BOOL)isSymbol:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isSymbol:(id)object withName:(NSString *)name {
    return [self isSymbol:object] ? [[(JSSymbol *)object value] isEqual:name] : NO;
}

/**
  Update the symbol with function info and function with the bounded symbol info.

  @param symbol A symbol.
  @param object The object bound to the symbol. The object will be updated in place if it is a function.
  @return symbol The symbol with updates if any
 */
+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object {
    if ([JSFunction isFunction:object]) {
        JSFunction *fn = (JSFunction *)object;
        [symbol setIsFunction:YES];
        [symbol setInitialArity:[fn argsCount]];
        [symbol resetArity];
        [fn setName:[symbol string]];
    }
    return symbol;
}

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
        _initialName = name;
        _arity = -2;
        _initialArity = -2;
        [self updateArity];
    }
    return self;
}

/** Initialise a symbol with function details if the given object is a function else returns the default symbol. */
- (instancetype)initWithFunction:(JSFunction *)func name:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
        _initialName = name;
        _initialArity = [func argsCount];
        [self resetArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity symbol:(JSSymbol *)symbol {
    return [self initWithArity:arity position:-1 symbol:symbol];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position symbol:(JSSymbol *)symbol {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = [symbol name];
        _initialName = [symbol initialValue];
        _meta = [symbol meta];
        _arity = arity;
        _initialArity = arity;
        _position = position;
        _hasNArity = [symbol hasNArity];
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string {
    return [self initWithArity:arity position:-1 string:string];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = string;
        _initialName = _name;
        _arity = arity;
        _initialArity = arity;
        _position = position;
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = [symbol name];
        _initialName = [symbol initialValue];
        _isFunction = [symbol isFunction];
        _hasNArity = [symbol hasNArity];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
        _initialName = name;
        _meta = meta;
        [self updateArity];
    }
    return self;
}

- (void)bootstrap {
    _position = -1;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"symbol";
}

- (BOOL)isGensym {
    return [_name isNotEqualTo:_initialName];
}

- (JSSymbol *)gensym {
    if (_position == 0) {
        _name = [[NSString alloc] initWithFormat:@"%@__%ld__auto__", [_name substringToIndex:[_name count]], [State counter]];
    }
    return self;
}

- (JSSymbol *)autoGensym {
    _name = [[NSString alloc] initWithFormat:@"%@__%ld__auto__", [_name substringToIndex:[_name count]], [State counter]];
    return self;
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSString *)name {
    return _name;
}

- (JSSymbol *)toNArity {
    _arity = -1;
    [self updateArity];
    return self;
}

- (JSSymbol *)resetArity {
    _arity = _initialArity;
    [self updateArity];
    return self;
}

- (void)updateArity {
    _hasNArity = _arity == -1;
    _isFunction = _arity >= -1;
}

- (NSString *)string {
    if (_initialArity <= -2) return _name;
    return [[NSString alloc] initWithFormat:@"%@/%@", _name, (_initialArity == -1) ? @"n" : [[NSString alloc] initWithFormat:@"%ld", _initialArity]];
}

- (void)copyProperties:(JSSymbol *)symbol {
    _initialArity = [symbol initialArity];
    _arity = _initialArity;
    _isFunction = [symbol isFunction];
    _meta = [symbol meta];
    _position = [symbol position];
    [self updateArity];
}

- (BOOL)isEqualToName:(NSString *)name {
    return [_name isEqual:name];
}

- (BOOL)isEqual:(id)symbol {
    if (![JSSymbol isSymbol:symbol]) return NO;
    return [_name isEqual:[symbol name]] && _arity == [symbol arity];
}

- (NSUInteger)hash {
    return [_name hash] + _arity + 2;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[JSSymbol alloc] initWithName:_name];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSSymbol alloc] initWithName:_name];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - %@ meta: %@>", NSStringFromClass([self class]), self, _name, _meta];
}

@end
