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

/** An env associated with a module. There is a global env and one specific for each module. */
@implementation Env {
    Env *_outer;
    /** Exported symbols for the module. If no module is defined, then the symbols are global. */
    ModuleTable *_exportTable;
    /** Imported symbols for the module. */
    ModuleTable *_importTable;
    /** The internal env table containing evaluated symbols with its binding. */
    ModuleTable *_internalTable;
    /** Used for auto gensym symbols */
    SymbolTable *_symbolTable;
    NSString *_moduleName;
    /** Is user defined module */
    BOOL _isUserDefined;
    BOOL _isExportAll;
}

@synthesize outer = _outer;
@synthesize exportTable = _exportTable;
@synthesize importTable = _importTable;
@synthesize internalTable = _internalTable;
@synthesize symbolTable = _symbolTable;
@synthesize moduleName = _moduleName;
@synthesize isUserDefined = _isUserDefined;
@synthesize isExportAll = _isExportAll;

#pragma mark Global modules lookup table

+ (void)initialize {
    if (self == [self class]) {
        _modules = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    }
}

/** Associates the env with the given module name. */
+ (void)setEnv:(Env *)env forModuleName:(NSString *)moduleName {
    [_modules setObject:env forKey:moduleName];
}

/** Returns env for the given module name. */
+ (Env *)forModuleName:(NSString *)moduleName {
    return [_modules objectForKey:moduleName];
}

/** Removes env for the given module name. */
+ (void)removeModule:(NSString *)moduleName {
    [_modules removeObjectForKey:moduleName];
}

/** Returns the internal modules table. */
+ (NSMapTable<NSString *, Env *> *)modules {
    return _modules;
}

#pragma mark Env

- (instancetype)initWithEnv:(Env *)env {
    self = [super init];
    if (self) {
        [self bootstrap];
        _moduleName = [env moduleName];
        _isExportAll = [env isExportAll];
        _isUserDefined = [env isUserDefined];
        _outer = env;
    }
    return self;
}

- (instancetype)initWithModuleName:(NSString *)name isUserDefined:(BOOL)isUserDefined {
    self = [super init];
    if (self) {
        [self bootstrap];
        _moduleName = name;
        _isUserDefined = isUserDefined;
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
    //[symbol setModuleName:[_module name]];
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
        _moduleName = [env moduleName];
        _outer = env;
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            if ([[sym name] isEqual:@"&"]) {
                JSSymbol *key = (JSSymbol *)binds[i + 1];
                if ([exprs count] > i) {
                    [self setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:key];
                } else {
                    JSList *list = [[JSList alloc] initWithArray:@[]];
                    [self setObject:list forKey:[self setFunctionInfo:list symbol:key]];
                }
                break;
            }
            [self setObject:exprs[i] forKey:[self setFunctionInfo:exprs[i] symbol:sym]];
        }
    }
    return self;
}

- (void)bootstrap {
    if (!_exportTable) _exportTable = [ModuleTable new];
    if (!_importTable) _importTable = [ModuleTable new];
    if (!_internalTable) _internalTable = [ModuleTable new];
    if (!_symbolTable) _symbolTable = [SymbolTable new];
    _moduleName = [Const defaultModuleName];
    _isExportAll = NO;
    _isUserDefined = YES;
}

- (id<JSDataProtocol>)objectForKey:(JSSymbol *)key {
    return [self objectForKey:key isThrow:YES];
}

- (id<JSDataProtocol>)objectForKey:(JSSymbol *)key isThrow:(BOOL)isThrow {
    if ([key isQualified]) return [self objectForKey:key inModule:[key initialModuleName]];
    NSString *moduleName = [key moduleName];
    id<JSDataProtocol> elem = nil;
    if ([moduleName isEqual:_moduleName]) {
        if (_isExportAll || [moduleName isEqual:[Const defaultModuleName]] || [moduleName isEqual:[Const coreModuleName]]) {
            elem = [_exportTable objectForSymbol:key];
            if (elem) return elem;
        } else {
            elem = [_internalTable objectForSymbol:key];
            if (elem) return elem;
        }
        elem = [_importTable objectForSymbol:key];
        if (elem) return elem;
        if ([self outer]) {
            elem = [_outer objectForKey:key isThrow:isThrow];
            if (elem) return elem;
        }
    } else {
        // Symbol belongs to another module
        elem = [self objectForKey:key inModule:[key moduleName]];
        if (elem) return elem;
    }
    // Symbol may belong to core
    [key setModuleName:[Const coreModuleName]];
    elem = [self objectForKey:key inModule:[Const coreModuleName]];
    if (elem) return elem;
    [key resetModuleName];
    if (isThrow) {
        [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    }
    return nil;
}

- (id<JSDataProtocol> _Nullable)objectForKey:(JSSymbol *)key inModule:(NSString *)name {
    Env *env = [Env forModuleName:name];
    JSSymbol *sym = key;
    if (env) {
        id<JSDataProtocol> elem;
        if ([key isQualified]) {
            [sym setModuleName:[key initialModuleName]];
        }
        if ([env isExportAll] || [name isEqual:[Const defaultModuleName]] || [name isEqual:[Const coreModuleName]]) {
            elem = [[env exportTable] objectForSymbol:sym];
            if (elem) return elem;
        } else {
            elem = [[env internalTable] objectForSymbol:sym];
            if (elem) return elem;
        }
        return [env outer] ? [[env outer] objectForKey:key] : nil;
    }
    return nil;
}

- (void)setObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    if (_isExportAll || [_moduleName isEqual:[Const defaultModuleName]] || [_moduleName isEqual:[Const coreModuleName]]) {
        [_exportTable setObject:obj forKey:key];
    } else {
        [_internalTable setObject:obj forKey:key];
    }
}

- (void)updateObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    if (_isExportAll || [_moduleName isEqual:[Const defaultModuleName]] || [_moduleName isEqual:[Const coreModuleName]]) {
        [_exportTable updateObject:obj forKey:key];
    } else {
        [_internalTable updateObject:obj forKey:key];
    }
}

- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p moduleName: %@, isExportAll: %hhd>", NSStringFromClass([self class]), self, [self moduleName], _isExportAll];
}

@end
