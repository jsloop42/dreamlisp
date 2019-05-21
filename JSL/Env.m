//
//  Env.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Env.h"

/** Holds the module envs loaded. */
static NSMapTable<NSString *, Env *> *_modules;
NSString *defaultModuleName = @"user";
NSString *coreModuleName = @"core";
NSString *currentModuleName;

/** An env associated with a module. There is a global env and one specific for each module. */
@implementation Env {
    Env *_outer;
    /** The env table containing evaluated symbols with its binding. */
    NSMapTable<JSSymbol *, id<JSDataProtocol>> *_table;
    /** Exported symbols for the module. If no module is defined, then the symbols are global. */
    ModuleTable *_module;
    /** Used for auto gensym symbols */
    SymbolTable *_symbolTable;
    /** Is user defined module */
    BOOL _isUserDefined;
    BOOL _isExportAll;
}

@synthesize outer = _outer;
@synthesize table = _table;
@synthesize module = _module;
@synthesize symbolTable = _symbolTable;
@synthesize isUserDefined = _isUserDefined;
@synthesize isExportAll = _isExportAll;

#pragma mark Module lookup table

+ (void)initialize {
    if (self == [self class]) {
        _modules = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        currentModuleName = defaultModuleName;
    }
}

+ (void)setEnv:(Env *)env forModuleName:(NSString *)moduleName {
    [_modules setObject:env forKey:moduleName];
}

+ (Env *)envForModuleName:(NSString *)moduleName {
    return [_modules objectForKey:moduleName];
}

+ (void)removeModule:(NSString *)moduleName {
    [_modules removeObjectForKey:moduleName];
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
        _isExportAll = [env isExportAll];
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
 Initializes environment with an outer environment and binds symbols with expressions. The current env is used instead of the symbol's env. Symbols can be
 qualified which refers to other env.

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
        if ([[env moduleName] isNotEqualTo:coreModuleName] && [currentModuleName isNotEqualTo:[env moduleName]]) {
            Env *currEnv = [Env envForModuleName:currentModuleName];
            _module = [currEnv module];
            _outer = currEnv;
            _isExportAll = [currEnv isExportAll];
        } else {
            _module = [env module];
            _outer = env;
            _isExportAll = [env isExportAll];
        }
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            if ([[sym name] isEqual:@"&"]) {
                JSSymbol *key = (JSSymbol *)binds[i + 1];
                [key setModuleName:[_module name]];
                if ([exprs count] > i) {
                    if (_isExportAll) {
                        [_module setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forSymbol:key];
                    } else {
                        [_table setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:key];
                    }
                } else {
                    if (_isExportAll) {
                        [_module setObject:[[JSList alloc] initWithArray:@[]] forSymbol:key];
                    } else {
                        [_table setObject:[[JSList alloc] initWithArray:@[]] forKey:key];
                    }
                }
                break;
            }
            if (_isExportAll) {
                [_module setObject:exprs[i] forSymbol:[self setFunctionInfo:exprs[i] symbol:sym]];
            } else {
                [_table setObject:exprs[i] forKey:[self setFunctionInfo:exprs[i] symbol:sym]];
            }
        }
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    if (!_module) _module = [ModuleTable new];
    _symbolTable = [SymbolTable new];
    _isExportAll = NO;
}

- (void)setObject:(id<JSDataProtocol>)obj forSymbol:(JSSymbol *)key {
    [key setModuleName:[_module name]];
    if (_isExportAll || [[self moduleName] isEqual:defaultModuleName] || [[self moduleName] isEqual:coreModuleName]) {
        [_module setObject:obj forSymbol:key];
    } else {
        [_table setObject:obj forKey:key];
    }
}

/** Checks if the symbol belongs to current module, if so update the module name. */
- (void)updateModuleName:(JSSymbol *)symbol {
    if (![symbol isQualified] && [[symbol moduleName] isNotEqualTo:currentModuleName]) {
        [symbol setModuleName:[_module name]];
    }
}

- (Env * _Nullable)findEnvForKey:(JSSymbol *)key inModule:(Env *)env {
    JSSymbol *aKey = [[JSSymbol alloc] initWithName:[key name]];
    [aKey copyProperties:key];
    [aKey setArity:[key arity]];
    [aKey updateArity];
    [aKey setModuleName:[env moduleName]];
    id<JSDataProtocol> obj = nil;
    obj = [[env module] objectForSymbol:aKey];
    if (obj) {
        return env;
    } else if (![aKey hasNArity]) {
        return [self findEnvForKey:[aKey toNArity] inModule:env];
    }
    return nil;
}

/**
 Lookup env table if the symbol belongs to current module, else lookup module table. Recursively checks the environments for the given symbol until a match
 is found or the environment is the outermost, which is nil.
 */
- (Env * _Nullable)findEnvForKey:(JSSymbol *)key inEnv:(Env *)env {
    if (!_isExportAll && [currentModuleName isEqual:[key moduleName]] && [currentModuleName isNotEqualTo:defaultModuleName] &&
        [currentModuleName isNotEqualTo:coreModuleName]) {
        if ([[env table] objectForKey:key]) {  // same module => all bindings are accessible
            return self;
        } else if (![key hasNArity]) {
            return [self findEnvForKey:[key toNArity] inEnv:env];
        }
    } else {
        if ([[env module] objectForSymbol:key]) {  // different module => look in module table only
            return self;
        } else if (![key hasNArity]) {
            return [self findEnvForKey:[key toNArity] inEnv:env];
        }
    }
    [key resetArity];
    return _outer ? [_outer findEnvForKey:key inEnv:_outer] : nil;
}

- (Env *)findEnvForKey:(JSSymbol *)key {
    [self updateModuleName:key];
    if ([key isQualified]) return [Env envForModuleName:[key moduleName]];
    Env *env = [self findEnvForKey:key inEnv:self];
    if (env) return env;
    env = [self findEnvForKey:key inModule:[Env envForModuleName:coreModuleName]];
    if (env) return env;
    return nil;
}

- (id<JSDataProtocol>)resolveFault:(id<JSDataProtocol>)object forKey:(JSSymbol *)key inEnv:(Env *)env {
    if ([JSFault isFault:object]) {
//        debug(@"Fault found for key: %@", key);
        id<JSDataProtocol> val = [[env table] objectForKey:key];
        if (val) {  // update object in export table
            [key setInitialModuleName:[env moduleName]];
            [[env module] setObject:val forSymbol:key];
            return val;
        } else {
            [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
        }
    }
    return object;
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key {
    Env *env = [self findEnvForKey:key];
    id<JSDataProtocol> val = nil;
    if (env) {
        val = [env objectForSymbol:key fromEnv:env];
        if (val) return [self resolveFault:val forKey:key inEnv:env];
    } else if ([[key moduleName] isEqual:[self moduleName]]) {
        env = self;
        val = [env objectForSymbol:key fromEnv:env];
        if (val) return [self resolveFault:val forKey:key inEnv:env];
    }
    // Check core module
    if (![key isQualified]) [key setModuleName:coreModuleName];
    val = [env objectForSymbol:key fromEnv:env];
    if (val) return [self resolveFault:val forKey:key inEnv:env];
    // Symbol not found
    if (![key isQualified]) [key setModuleName:currentModuleName];
    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return nil;
}

- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key fromEnv:(Env *)env {
    id<JSDataProtocol> val = nil;
    if (!_isExportAll && [currentModuleName isEqual:[key moduleName]] && [[key moduleName] isNotEqualTo:defaultModuleName] && [[key moduleName] isNotEqualTo:coreModuleName]) {
        val = [[env table] objectForKey:key];
        if (val) {
            return val;
        } else if (![key hasNArity]) {
            return [self objectForSymbol:[key toNArity] fromEnv:env];
        }
    } else {
        val = [[env module] objectForSymbol:key];
        if (val) {
            return val;
        } else if (![key hasNArity]) {
            return [self objectForSymbol:[key toNArity] fromEnv:env];
        } else if ([[key moduleName] isEqual:defaultModuleName] || [[key moduleName] isEqual:coreModuleName]) {
            [key resetArity];
            val = [self objectForSymbol:key fromTable:env];
            if (val) return val;
        }
    }
    [key resetArity];
    return [env outer] ? [self objectForSymbol:key fromEnv:[env outer]]: nil;
}

- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key fromTable:(Env *)env {
    id<JSDataProtocol> val = nil;
    val = [[env table] objectForKey:key];
    if (val) {
        return val;
    } else if (![key hasNArity]) {
        return [self objectForSymbol:[key toNArity] fromTable:env];
    }
    [key resetArity];
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
