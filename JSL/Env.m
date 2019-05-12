//
//  Env.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Env.h"

/** An env associated with a module. There is a global env and one specific for each module. */
@implementation Env {
    Env *_outer;
    /** The env table containing evaluated symbols with its binding. */
    NSMapTable<JSSymbol *, id<JSDataProtocol>> *_table;
    /** Exported symbols for the module. If no module is defined, then the symbols are global. */
    ModuleTable *_module;
    /** The core module */
    ModuleTable *_coreModule;
    BOOL _isModule;
}

@synthesize outer = _outer;
@synthesize table = _table;
@synthesize coreModule = _coreModule;
@synthesize module = _module;
@synthesize isModule = _isModule;

- (instancetype)initWithCoreModule:(ModuleTable *)core {
    self = [super init];
    if (self) {
        _coreModule = core;
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithEnv:(Env *)env {
    self = [super init];
    if (self) {
        _coreModule = [env coreModule];
        _module = [env module];
        [self bootstrap];
        _outer = env;
    }
    return self;
}

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

/** If the symbol is associated with a function, update the symbol with function details. */
- (JSSymbol *)setFunctionInfo:(id<JSDataProtocol>)object symbol:(JSSymbol *)symbol {
    if ([JSFunction isFunction:object]) {
        [symbol setIsFunction:YES];
        [symbol setArity:[(JSFunction *)object argsCount]];
    }
    return symbol;
}

/**
 Initializes environment with an outer environment and binds symbols with expressions.

 @param env The outer environment.
 @param binds A array of `JSSymbol` symbols.
 @param exprs A array of `id<JSDataProtocol>` expressions.
 */
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs {
    self = [super init];
    NSUInteger len = [binds count];
    NSUInteger i = 0;
    if (self) {
        [self bootstrap];
        _coreModule = [env coreModule];
        _outer = env;
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            if ([[sym name] isEqual:@"&"]) {
                if ([exprs count] > i) {
                    [_table setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:(JSSymbol *)binds[i + 1]];
                } else {
                    [_table setObject:[[JSList alloc] initWithArray:@[]] forKey:(JSSymbol *)binds[i + 1]];
                }
                break;
            }
            [_table setObject:exprs[i] forKey:[self setFunctionInfo:exprs[i] symbol:sym]];
        }
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    if (!_module) _module = [ModuleTable new];
}

- (void)setObject:(id<JSDataProtocol>)obj forSymbol:(JSSymbol *)key {
    [_table setObject:obj forKey:key];
}

/** Recursively checks the environments for the given symbol until a match is found or the environment is the outermost, which is nil. */
- (Env *)findEnvForKey:(JSSymbol *)key {
    if ([_table objectForKey:key]) {
        return self;
    } else if (![key hasNArity]) {
        return [self findEnvForKey:[key toNArity]];
    }
    return _outer ? [_outer findEnvForKey:[key resetArity]] : nil;
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key {
    return [self objectForSymbol:key isFromCore:NO];
}

/** Retrieves the matching element for the given key from the environment if found. If not checks the @c core module. Else throws an exception. */
- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key isFromCore:(BOOL)isFromCore {
    Env *env = [self findEnvForKey:key];
    id<JSDataProtocol>val = nil;
    if (env != nil) {
        val = [[env table] objectForKey:key];
        if (val != nil) return val;
    }
    // Check for n arity symbol
    if (![key hasNArity]) return [self objectForSymbol:[key toNArity]];
    if (!isFromCore) {
        val = [self objectForSymbolFromCore:[key resetArity]];
        if (val) return val;
    }
    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return nil;
}

/** Retrieves object from @c core module if present. */
- (_Nullable id<JSDataProtocol>)objectForSymbolFromCore:(JSSymbol *)key {
    if (!_coreModule) [[[JSError alloc] initWithFormat:ModuleEmpty, @"'core'"] throw];
    id<JSDataProtocol> obj = [_coreModule objectForSymbol:key];
    if (obj) {
        return obj;
    } else if (![key hasNArity]) {
        return [self objectForSymbolFromCore:[key toNArity]];
    }
    return nil;
}

@end
