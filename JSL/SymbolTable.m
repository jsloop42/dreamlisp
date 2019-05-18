//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

@implementation SymTableKey {
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

- (instancetype)initWithKey:(SymTableKey *)key {
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
    return [[SymTableKey alloc] initWithKey:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[SymTableKey alloc] initWithKey:self];
}

@end

/**
 A table used for storing and lookup of hygienic symbols for an REP loop. The table will contain symbols encountered which are defined using def!, defmacro!.
 Since processing of macros does not happen at auto gensym stage, any symbols defined using macro functions other than defmacro! will not be added.
 */
@implementation SymbolTable {
    SymbolTable *_outer;
    NSMapTable<SymTableKey *, JSSymbol *> *_table;
}

@synthesize outer = _outer;
@synthesize table = _table;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithTable:(SymbolTable *)table {
    self = [super init];
    if (self) {
        [self bootstrap];
        _outer = table;
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (void)merge:(SymbolTable *)table {
    [_table merge:[table table]];
    if ([table outer]) [_outer merge:[table outer]];
}

- (SymTableKey *)toKey:(JSSymbol *)symbol {
    return [[SymTableKey alloc] initWithSymbol:symbol];
}

- (JSSymbol * _Nullable)symbolForKey:(JSSymbol *)symbol inTable:(SymbolTable *)symTable {
    if (!symTable) return nil;
    JSSymbol *sym = [[symTable table] objectForKey:[self toKey:symbol]];
    return sym ? sym : [self symbolForKey:symbol inTable:[symTable outer]];
}

- (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol {
    return [self symbolForKey:symbol inTable:self];
}

- (void)setSymbol:(JSSymbol *)symbol {
    [_table setObject:symbol forKey:[self toKey:symbol]];
}

- (void)clearAll {
    [_table removeAllObjects];
    if (_outer) [_outer clearAll];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSString *)description {
    return [_table description];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[SymbolTable alloc] initWithTable:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[SymbolTable alloc] initWithTable:self];
}

@end
