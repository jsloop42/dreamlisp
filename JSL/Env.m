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
    // To distinguish it from JSFunction's env which may be different from current env if the function is referenced from another module. If the env is
    // imported into the current env. Encountered within initWithEnv:binds:exprs:IsImported.. method.
    BOOL _isImported;
}

@synthesize outer = _outer;
@synthesize table = _table;
@synthesize module = _module;
@synthesize symbolTable = _symbolTable;
@synthesize isUserDefined = _isUserDefined;
@synthesize isExportAll = _isExportAll;
@synthesize isImported = _isImported;

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
    return [self initWithEnv:env binds:binds exprs:exprs isImported:NO currentEnv:env];
}

- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs isImported:(BOOL)isImported currentEnv:(Env *)currentEnv {
    self = [super init];
    NSUInteger len = [binds count];
    NSUInteger i = 0;
    if (self) {
        [self bootstrap];
        _isImported = isImported;
        Env *effEnv = nil;
        if (isImported) {
            effEnv = currentEnv;
        } else if ([[env moduleName] isEqual:[currentEnv moduleName]]) {
            // Eg: nested functions have same module but env depth will be more than the currentEnv due to [fn env] value.
            effEnv = env;
        } else if (env != currentEnv && [[env moduleName] isNotEqualTo:coreModuleName]) {  // core.jsl symbols accessed => effEnv = core
            effEnv = currentEnv;
        } else {
            effEnv = env;
        }
        [_module setName:[effEnv moduleName]];
        _outer = effEnv;
        _isExportAll = [effEnv isExportAll];
        _isUserDefined = [effEnv isUserDefined];
        // Sets module name to all binds element to current module name if they does not belong to core.
        [binds enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self updateModuleNameForExprs:obj moduleName:currentModuleName];
        }];
        // Sets module name to all symbols in the exprs array which will set the params list for functions to belong to same module as the function.
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
//        if ([[self moduleName] isEqual:coreModuleName]) {
//            [key setInitialModuleName:coreModuleName];
//            [key resetModuleName];
//            NSAssert([[key moduleName] isEqual:coreModuleName], @"Core symbols should have core as its module name");
//        }
        [_module setObject:obj forSymbol:key];
        NSAssert([_module objectForSymbol:key] != nil, @"cannot be nil");
        if ([[key name] isEqual:@"defmacro"]) {
            NSAssert([[key moduleName] isEqual:coreModuleName], @"wrong module name");
        }
    } else {
        id <JSDataProtocol> elem = [_table objectForKey:key];
        if (elem) {
            // FIXME: if imported symbol is getting overwritten, display a warning.
        }
        [_table setObject:obj forKey:key];
    }
}

/** Checks if the symbol belongs to current module, if so update the module name. */
- (void)updateModuleName:(JSSymbol *)symbol {
    if (![symbol isQualified] && ![symbol isImported] && [[symbol moduleName] isNotEqualTo:currentModuleName]) {
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
        if (_isImported && [modName isNotEqualTo:coreModuleName]) {
            [sym setInitialModuleName:[sym moduleName]];
            [sym setModuleName:moduleName];
            [sym setIsImported:YES];
        } else if (![sym isQualified] && [modName isNotEqualTo:moduleName] && [modName isNotEqualTo:coreModuleName] &&
                   [[sym moduleName] isNotEqualTo:[sym initialModuleName]]) {
            // These can be imported symbols or symbols defined in a new module
            [sym setModuleName:moduleName];
            id<JSDataProtocol> elem = [self objectForSymbol:sym isThrow:NO];
            if (elem && [elem isImported]) {
                [sym setIsImported:YES];  // symbols module name should point to current module and initial module name to the imported module name.
            }
            if ([sym isImported]) {
                [sym setInitialModuleName:[elem moduleName]];
            } else {
                [sym setInitialModuleName:[sym moduleName]];
            }
        }
//        } else if (![sym isQualified] && [modName isEqual:moduleName] && [modName isNotEqualTo:coreModuleName]) {
//            // Inner bindings of a function. In `(defun greet () (sum 11))`, the inner bindings starts from `(sum 11)`. These are present as ast in a JSFunction
//            // object.
//            [sym setModuleName:moduleName];
//            id<JSDataProtocol> elem = [self objectForSymbol:sym isThrow:NO];
//            if (elem && [elem isImported]) {
//                [sym setIsImported:YES];
//            }
//            if ([sym isImported]) {
//                [sym setInitialModuleName:[elem moduleName]];
//            } else {
//                [sym setInitialModuleName:[sym moduleName]];
//            }
//        }
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
        [aKey setInitialModuleName:[key initialModuleName]];
        [aKey setInitialArity:[aKey arity]];
        key = aKey;
        return env;
    } else if (![aKey hasNArity]) {
        return [self findEnvForKey:[aKey toNArity] inModule:env];
    }
    return [env outer] ? [self findEnvForKey:key inModule:[env outer]] : nil;
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
        if ([[env module] objectForSymbol:key]) {  // different module, core or user module => look in module table only
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
    if ([key isQualified]) {
        return [Env envForModuleName: [key isImported] ? [key initialModuleName] : [key moduleName]];
    } else if ([key isImported]) {
        return [Env envForModuleName: [key initialModuleName]];
    }
    Env *env = [self findEnvForKey:key inEnv:self];
    if (env) return env;
    env = [self findEnvForKey:key inModule:[Env envForModuleName:coreModuleName]];
    if (env) return env;
    return nil;
}

- (id<JSDataProtocol>)resolveFault:(id<JSDataProtocol>)object forKey:(JSSymbol *)key inEnv:(Env *)env {
    if ([JSFault isFault:object]) {
        JSFault *fault = (JSFault *)object;
        id<JSDataProtocol> val = nil;
        if ([fault isImported]) {
            Env *modEnv = [Env envForModuleName:[fault moduleName]];
            if (modEnv) {
                [key setModuleName:[fault moduleName]];
                val = [[modEnv module] objectForSymbol:key];
                // An imported symbol with fault found. The symbol value's from the original module can also be a fault if the export was not resolved already.
                // Check if the export needs to be resolved.
                val = [[self resolveFault:val forKey:key inEnv:modEnv] mutableCopyWithZone:nil];
                if (val) {
                    [val setIsImported:YES];
                    [key setIsImported:YES];
                    [key setInitialModuleName:[modEnv moduleName]];
                    [key setModuleName:[env moduleName]];
                    [key setIsQualified:YES];  // Can be set to NO, in which case lookup would take additional steps to resolve.
                    [[env table] setObject:val forKey:key];
                    return val;
                } else {
                    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
                }
            }
        } else {  // Exported symbol => fetch from module table.
            val = [self objectForSymbol:key fromTable:env];
            if (val) {  // update object in export table
                [key setInitialModuleName:[env moduleName]];
                [key setIsFault:NO];
                // The original function changes are reflected for imports as well because the function if fetch from the referring module table if it is either
                // qualified or imported.
                [key setIsQualified:YES];  // Can be set to NO
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
        if ([[env moduleName] isEqual:coreModuleName]) {
            [key setModuleName:coreModuleName];
            val = [env objectForSymbol:key fromEnv:env];
        } else {
            val = [self objectForSymbol:key fromEnv:env];
        }
        if (val) return [self resolveFault:val forKey:key inEnv:env];
    } else if ([[key moduleName] isEqual:[self moduleName]]) {
        env = self;
        val = [self objectForSymbol:key fromEnv:env];
        if (val) return [self resolveFault:val forKey:key inEnv:env];
    }
    // Check core module
    if (![key isQualified]) [key setModuleName:coreModuleName];
    val = [self objectForSymbol:key fromEnv:env];
    if (val) return [self resolveFault:val forKey:key inEnv:env];
    // Symbol not found
    if (![key isQualified]) [key setModuleName:currentModuleName];
    //info(@"Symbol not found: %@", [key string]);
    if (isThrow) {
        if ([[key name] isEqual:@"defmacro"]) {
            [self test];
        }
        [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    }
    return nil;
}

- (void)test {
    Env *coreEnv = [Env envForModuleName:coreModuleName];
    NSMutableArray *keys = [[[[coreEnv module] table] allKeys] mutableCopy];
    [keys enumerateObjectsUsingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        if ([JSSymbol isSymbol:obj withName:@"defmacro"]) {
            JSSymbol *sym = (JSSymbol *)obj;
            info(@"%@", sym);
            id<JSDataProtocol> val = [[[coreEnv module] table] objectForKey:sym];
            info(@"%@", val);
            [sym setModuleName:coreModuleName];
        }
    }];
}

- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key fromEnv:(Env *)env {
    id<JSDataProtocol> val = nil;
    // Resolve import and export to latest symbol value, which makes hot code reload reflect changes throughout the system seamlessly
    if (([key isQualified] && [key isImported]) || [key isImported]) {
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
