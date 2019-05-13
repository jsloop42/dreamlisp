//
//  Env.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Env.h"

static NSMapTable<NSString *, Env *> *_modules;
NSString *defaultModuleName = @"user";
NSString *coreModuleName = @"core";

/** An env associated with a module. There is a global env and one specific for each module. */
@implementation Env {
    Env *_outer;
    /** The env table containing evaluated symbols with its binding. */
    NSMapTable<JSSymbol *, id<JSDataProtocol>> *_table;
    /** Exported symbols for the module. If no module is defined, then the symbols are global. */
    ModuleTable *_module;
    /** Is user defined module */
    BOOL _isUserDefined;
}

@synthesize outer = _outer;
@synthesize table = _table;
@synthesize module = _module;
@synthesize isUserDefined = _isUserDefined;

#pragma mark Module lookup table

+ (void)initialize {
    if (self == [self class]) {
        _modules = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    }
}

+ (void)setEnv:(Env *)env forModuleName:(NSString *)moduleName {
    [_modules setObject:env forKey:moduleName];
}

+ (Env *)envForModuleName:(NSString *)moduleName {
    return [_modules objectForKey:moduleName];
}

+ (NSMapTable<NSString *, Env *> *)modules {
    return _modules;
}

#pragma mark Env table

- (instancetype)initWithModuleName:(NSString *)name isUserDefined:(BOOL)isUserDefined {
    self = [super init];
    if (self) {
        [self bootstrap];
        [_module setName:name];
        _isUserDefined = isUserDefined;
    }
    return self;
}

- (instancetype)initWithEnv:(Env *)env {
    self = [super init];
    if (self) {
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
    [symbol setModuleName:[_module name]];
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
        _outer = env;
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            JSSymbol *key = (JSSymbol *)binds[i + 1];
            [key setModuleName:[_module name]];
            if ([[sym name] isEqual:@"&"]) {
                if ([exprs count] > i) {
                    [_table setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:key];
                } else {
                    [_table setObject:[[JSList alloc] initWithArray:@[]] forKey:key];
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
    [key setModuleName:[_module name]];
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
    return [self objectForSymbol:key isFromModuleTable:NO];
}

/** Retrieves the matching element for the given key from the environment if found. If not checks the @c modules' table. Else throws an exception. */
- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key isFromModuleTable:(BOOL)isFromModuleTable {
    Env *env = [self findEnvForKey:key];
    id<JSDataProtocol>val = nil;
    if (env != nil) {
        val = [[env table] objectForKey:key];
        if (val != nil) return val;
    }
    // Check for n arity symbol
    if (![key hasNArity]) return [self objectForSymbol:[key toNArity]];
    if (!isFromModuleTable) {
        val = [self objectForSymbolFromModuleTable:[key resetArity]];
        if (val) return val;
    }
    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return nil;
}

/**
 Retrieves object from @c modules' table if present. If not fully qualified the symbol is checked against core env because core modules need not be fully
 qualified */
- (_Nullable id<JSDataProtocol>)objectForSymbolFromModuleTable:(JSSymbol *)key {
    if (![key isQualified]) [key setModuleName:coreModuleName];  // set module to core
    // Fetch from modules table
    Env *modEnv = [Env envForModuleName:[key moduleName]];
    id<JSDataProtocol>val = nil;
    if (modEnv) {
        val = [[modEnv table] objectForKey:key];
        if (val != nil) return val;
        // Check for n arity symbol
        if (![key hasNArity]) return [self objectForSymbolFromModuleTable:[key toNArity]];
    }
    return nil;
}

#pragma mark Module

- (void)setModuleName:(NSString *)name {
    [_module setName:name];
}

- (NSString *)moduleName {
    return [_module name];
}

@end
