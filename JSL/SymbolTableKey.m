//
//  SymbolTableKey.m
//  JSL
//
//  Created by jsloop on 18/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTableKey.h"

@implementation SymbolTableKey {
    NSString *_initialValue;
    NSInteger _arity;
    BOOL _isFunction;
    BOOL _hasNArity;
    NSInteger _position;
    /** Function name with arity if the symbol is bound to a function */
    NSString *_fnName;
    NSString *_moduleName;
    BOOL _isQualified;
    BOOL _isModule;
}

@synthesize arity = _arity;
@synthesize initialValue = _initialValue;
@synthesize moduleName = _moduleName;

- (instancetype)initWithSymbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        _arity = [symbol arity];
        _initialValue = [symbol initialValue];
        _moduleName = [symbol moduleName];
    }
    return self;
}

- (instancetype)initWithKey:(SymbolTableKey *)key {
    self = [super self];
    if (self) self = key;
    return self;
}

- (BOOL)isEqual:(id)symbol {
    return [_initialValue isEqual:[symbol initialValue]] && _arity == [symbol arity] && [_moduleName isEqual:[symbol moduleName]];
}

- (NSUInteger)hash {
    return [_moduleName hash] + [_initialValue hash] + _arity + 2;  // Adding 2 to offset negative arity.
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - M:%@ %@ Arity:%ld>", NSStringFromClass([self class]), self, _moduleName, _initialValue, _arity];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[SymbolTableKey alloc] initWithKey:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[SymbolTableKey alloc] initWithKey:self];
}

@end
