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
        _isUserDefined = [env isUserDefined];
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
    return [self initWithEnv:env binds:binds exprs:exprs isImported:NO];
}

- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs isImported:(BOOL)isImported {
    self = [super init];
    NSUInteger len = [binds count];
    NSUInteger i = 0;
    if (self) {
        [self bootstrap];

//        if ([[env moduleName] isNotEqualTo:coreModuleName] && [currentModuleName isNotEqualTo:[env moduleName]]) {
//            Env *currEnv = [Env envForModuleName:currentModuleName];
//            _module = [currEnv module];
//            _outer = currEnv;
//            _isExportAll = [currEnv isExportAll];
//            _isUserDefined = [currEnv isUserDefined];
//        } else {
//            _module = [env module];
//            _outer = env;
//            _isExportAll = [env isExportAll];
//            _isUserDefined = [env isUserDefined];
//        }
        [_module setName:[env moduleName]];
        _outer = env;
        _isExportAll = [env isExportAll];
        _isUserDefined = [env isUserDefined];
        // Set module name to all symbols in the exprs array which will set the params list for functions to belong to same module as the function.
        [exprs enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self updateModuleNameForExprs:obj moduleName:currentModuleName];
        }];
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            if ([[sym name] isEqual:@"&"]) {
                JSSymbol *key = (JSSymbol *)binds[i + 1];
                [key setModuleName:[_module name]];
                if ([exprs count] > i) {
                    [self setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forSymbol:key];
                } else {
                    JSList *list = [[JSList alloc] initWithArray:@[]];
                    [self setObject:list forSymbol:[self setFunctionInfo:list symbol:key]];
                }
                break;
            }
            [self setObject:exprs[i] forSymbol:[self setFunctionInfo:exprs[i] symbol:sym]];
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
        id <JSDataProtocol> elem = [_table objectForKey:key];
        if (elem) {
             // if imported symbol is getting overwritten, display a warning.
        }
        [_table setObject:obj forKey:key];
    }
}

/** Checks if the symbol belongs to current module, if so update the module name. */
- (void)updateModuleName:(JSSymbol *)symbol {
    if (![symbol isQualified] && [[symbol moduleName] isNotEqualTo:currentModuleName]) {
        [symbol setModuleName:[_module name]];
    }
}

/** Update module name for expressions that corresponds to bindings. Invoked when any of the fn*, let*, catch* and function ast application is encountered.  */
- (void)updateModuleNameForExprs:(id<JSDataProtocol>)ast moduleName:(NSString *)moduleName {
    if ([JSList isList:ast]) {
        JSList *xs = (JSList *)ast;
        JSSymbol *sym = (JSSymbol *)[xs first];
        if ([xs position] >= 3) {
            [sym setArity:[xs count] - 1];
            [sym setInitialArity:[sym arity]];
            [sym updateArity];
        }
        [(NSMutableArray *)[xs value] enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self updateModuleNameForExprs:obj moduleName:moduleName];
        }];
    } if ([JSVector isVector:ast]) {
        [(NSMutableArray *)[(JSVector *)ast value] enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self updateModuleNameForExprs:obj moduleName:moduleName];
        }];
    } else if ([JSSymbol isSymbol:ast]) {
        JSSymbol *sym = (JSSymbol *)ast;
        NSString *modName = [sym moduleName];
        if (![sym isQualified] && [modName isNotEqualTo:moduleName] && [modName isNotEqualTo:coreModuleName]) {
            [sym setModuleName:moduleName];
            id<JSDataProtocol> elem = [self objectForSymbol:sym isThrow:NO];
            if (elem && [elem isImported]) {
                [sym setIsImported:YES];
                // TODO: set module name?
            }
            if ([sym isImported]) {
                [sym setInitialModuleName:moduleName];
            }
        }
    } else if ([JSHashMap isHashMap:ast]) {
        NSMutableDictionary *hm = [(JSHashMap *)ast value];
        NSMutableArray *allKeys = [[hm allKeys] mutableCopy];
        NSMutableArray *allVals = [[hm allValues] mutableCopy];
        [allKeys enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self updateModuleNameForExprs:obj moduleName:moduleName];
        }];
        [allVals enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self updateModuleNameForExprs:obj moduleName:moduleName];
        }];
    } else if ([JSAtom isAtom:ast]) {
        [self updateModuleNameForExprs:[(JSAtom *)ast value] moduleName:moduleName];
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
    //Env *env = [self findEnvForKey:key inEnv:[env moduleName]];
    [self updateModuleName:key];
    if ([key isQualified]) return [Env envForModuleName: [key isImported] ? [key initialModuleName] : [key moduleName]];
    Env *env = [self findEnvForKey:key inEnv:self];
    if (env) return env;
    env = [self findEnvForKey:key inModule:[Env envForModuleName:coreModuleName]];
    if (env) return env;
    return nil;
}

- (id<JSDataProtocol>)resolveFault:(id<JSDataProtocol>)object forKey:(JSSymbol *)key inEnv:(Env *)env {
    if ([JSFault isFault:object]) {
//        debug(@"Fault found for key: %@", key);
        JSFault *fault = (JSFault *)object;
        id<JSDataProtocol> val = nil;
        if ([fault isImported]) {
            Env *modEnv = [Env envForModuleName:[fault moduleName]];
            if (modEnv) {
                [key setModuleName:[fault moduleName]];
                val = [[modEnv module] objectForSymbol:key];
                val = [[self resolveFault:val forKey:key inEnv:modEnv] mutableCopyWithZone:nil];
                if (val) {
                    [val setIsImported:YES];
                    [key setIsImported:YES];
                    [key setInitialModuleName:[modEnv moduleName]];
                    [key setModuleName:[env moduleName]];
                    [[env table] setObject:val forKey:key];
                    return val;
                } else {
                    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
                }
            }
        } else {  // Exported symbol => fetch from module table.
            //val = [[env table] objectForKey:key];
            val = [self objectForSymbol:key fromTable:env];
            if (val) {  // update object in export table
                [key setInitialModuleName:[env moduleName]];
                [key setIsFault:NO];
                [key setIsQualified:YES];  // TODO: Is this required ?
                [[env module] setObject:val forSymbol:key];
                return val;
            } else {
                [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
            }
        }
    }
    return object;
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key {
    return [self objectForSymbol:key isThrow:YES];
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key isThrow:(BOOL)isThrow {
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
    // TODO: isQualified and isImported
    // Check core module
    if (![key isQualified]) [key setModuleName:coreModuleName];
    val = [env objectForSymbol:key fromEnv:env];
    if (val) return [self resolveFault:val forKey:key inEnv:env];
    // Symbol not found
    if (![key isQualified]) [key setModuleName:currentModuleName];
    if (isThrow) [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return nil;
}

- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key fromEnv:(Env *)env {
    id<JSDataProtocol> val = nil;
    if ([key isQualified] && [key isImported]) {
        return [self objectForSymbol:key fromImportedEnv:[Env envForModuleName:[key initialModuleName]]];
    }
    if (!_isExportAll && [currentModuleName isEqual:[key moduleName]] && [[key moduleName] isNotEqualTo:defaultModuleName] &&
        [[key moduleName] isNotEqualTo:coreModuleName]) {
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

/** Fetch imported symbol from env table */
- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key fromImportedEnv:(Env *)env {
    id<JSDataProtocol> val = nil;
    JSSymbol *sym = key;
    [sym setModuleName:[sym initialModuleName]];
    return [self objectForSymbol:sym fromTable:env];
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
    return [env outer] ? [self objectForSymbol:key fromTable:[env outer]]: nil;
}

#pragma mark Module

- (void)setModuleName:(NSString *)name {
    [_module setName:name];
}

- (NSString *)moduleName {
    return [_module name];
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p moduleName: %@, isExportAll: %hhd>", NSStringFromClass([self class]), self, [self moduleName], _isExportAll];
}

@end
