//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 22/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

#pragma mark Symbol key

@implementation SymbolKey {
    NSString *_name;
    NSString *_moduleName;
}

@synthesize name = _name;
@synthesize moduleName = _moduleName;

+ (SymbolKey *)fromSymbol:(JSSymbol *)symbol {
    return [[SymbolKey alloc] initWithName:[symbol value] moduleName:[symbol moduleName]];
}

- (instancetype)initWithName:(NSString *)name moduleName:(NSString *)moduleName {
    self = [super init];
    if (self) {
        _name = name;
        _moduleName = moduleName;
    }
    return self;
}

- (BOOL)isEqual:(id)object {
    if (![object isKindOfClass:[self class]]) return NO;
    SymbolKey *key = (SymbolKey *)object;
    return [_name isEqual:[key name]] && [_moduleName isEqual:[key moduleName]];
}

- (NSUInteger)hash {
    return [_name hash] + [_moduleName hash];
}

@end

#pragma mark - Symbol table

/**
 A class used to keep track of objects that uses same binding name. Symbol table uses @c SymbolKey for its key and @c JSSymbol as its value. Cases where
 function takes a function as its argument and returns or passes the argument to another function without annotating symbol with arity information fails as
 function bindings and data bindings are different due to arity info. The symbol table lookup mitigates this by choosing a bounded symbol from the current
 environment.

 (defun identity (x) x)
 (identity (fn (n) n)) ; #<fn/1>
 (identity (fn (a b) 0)) ; #<fn/2>

 Here the x in identity is bound to the anonymous function, and the body x is a symbol with -2 arity.
 */
@implementation SymbolTable {
    NSMapTable <SymbolKey *, NSMutableArray<JSSymbol *> *> *_table;
}

@synthesize table = _table;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

/** Retrieves the array associated with the symbol key. */
- (NSMutableArray * _Nullable)arrayForKey:(SymbolKey *)key {
    return [_table objectForKey:key];
}

/** Adds the given symbol in the array corresponding to the symbol key. */
- (void)setKey:(JSSymbol *)key {
    SymbolKey *symKey = [SymbolKey fromSymbol:key];
    NSMutableArray *arr = [self arrayForKey:symKey];
    if (!arr) arr = [NSMutableArray new];
    if (![arr containsObject:key]) {
        [arr addObject:key];
        [_table setObject:arr forKey:symKey];
    }
}

/**
 Returns an appropriate symbol with arity for the given key. If the given symbol exists, it is returned. Else symbol with arity -2 is returned if present,
 else a function symbol with the largest arity is returned if present. If non is found, nil is returned. */
- (id<JSDataProtocol> _Nullable)symbolForKey:(JSSymbol *)key {
    NSMutableArray *arr = [self arrayForKey:[SymbolKey fromSymbol:key]];
    if (!arr) return nil;
    if ([arr containsObject:key]) return key;
    JSSymbol *sym = [self dataSymbolForKey:key symbolArray:arr];
    if (sym) return sym;
    return [self functionSymbolForKey:key symbolArray:arr];
}

/** Returns the symbol with -2 arity if present. */
- (JSSymbol * _Nullable)dataSymbolForKey:(JSSymbol *)key symbolArray:(NSMutableArray *)array {
    JSSymbol *sym = [key copy];
    [sym setArity:-2];
    if ([array containsObject:sym]) return sym;
    return nil;
}

/** Returns the symbol with largest function arity if present. */
- (JSSymbol * _Nullable)functionSymbolForKey:(JSSymbol *)key symbolArray:(NSMutableArray *)array {
    [array sortUsingComparator:^NSComparisonResult(id  _Nonnull obj1, id  _Nonnull obj2) {
        return [JSSymbol compareSymbol:obj1 withSymbol:obj2];
    }];
    return [array first];
}

@end
