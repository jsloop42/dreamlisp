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
    id<JSDataProtocol> _meta;
    NSInteger _arity;
    NSInteger _initialArity;
    BOOL _isFunction;
    BOOL _hasNArity;
}

@synthesize arity = _arity;
@synthesize initialArity = _initialArity;
@synthesize isFunction = _isFunction;
@synthesize hasNArity = _hasNArity;
@synthesize meta = _meta;
@synthesize value = _name;

+ (BOOL)isSymbol:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isSymbol:(id)object withName:(NSString *)name {
    return [self isSymbol:object] ? [[(JSSymbol *)object value] isEqual:name] : NO;
}

+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object {
    return [JSFunction isFunction:object] ? [[JSSymbol alloc] initWithArity:[(JSFunction *)object argsCount] symbol:symbol] : symbol;
}

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
        _arity = -2;
        _initialArity = -2;
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity symbol:(JSSymbol *)symbol {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = [symbol name];
        _meta = [symbol meta];
        _arity = arity;
        _initialArity = arity;
        _isFunction = YES;
        _hasNArity = [symbol hasNArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = string;
        _arity = arity;
        _initialArity = arity;
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        _name = [symbol name];
        _isFunction = [symbol isFunction];
        _hasNArity = [symbol hasNArity];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
        _meta = meta;
        [self updateArity];
    }
    return self;
}

- (void)updateArity {
    _hasNArity = _arity == -1;
    _isFunction = _arity >= -1;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"symbol";
}

- (NSString *)name {
    return _name;
}

- (NSString *)value {
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

- (NSString *)string {
    if (_initialArity <= -2) return _name;
    return [[NSString alloc] initWithFormat:@"%@/%@", _name, (_initialArity == -1) ? @"n" : [[NSString alloc] initWithFormat:@"%ld", _initialArity]];
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
