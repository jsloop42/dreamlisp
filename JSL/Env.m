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
    NSString *_moduleName;
    NSString *_moduleDescription;
    /** Is user defined module */
    BOOL _isUserDefined;
    BOOL _isExportAll;
}

@synthesize outer = _outer;
@synthesize exportTable = _exportTable;
@synthesize importTable = _importTable;
@synthesize internalTable = _internalTable;
@synthesize moduleName = _moduleName;
@synthesize moduleDescription = _moduleDescription;
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
        _isExportAll = [env isExportAll];
        _outer = env;
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            if ([[sym value] isEqual:@"&"]) {
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
    _moduleName = [Const defaultModuleName];
    _moduleDescription = @"";
    _isExportAll = NO;
    _isUserDefined = YES;
}

- (id<JSDataProtocol>)resolveImportFault:(JSFault *)fault forKey:(JSSymbol *)key inEnv:(Env *)env {
    id<JSDataProtocol> val = nil;
    Env *modEnv = [Env forModuleName:[fault moduleName]];
    JSSymbol *sym = [key copy];
    if (modEnv) {
        [sym setModuleName:[fault moduleName]];  // update module name so that the key can be retrieved from the original module
        val = [[modEnv exportTable] objectForSymbol:sym];
        if (val) {
            [val setIsImported:YES];
            [key setIsImported:YES];
            [key setIsFault:NO];
            [key setInitialModuleName:[modEnv moduleName]];
            [key setModuleName:[env moduleName]];
            [key setIsQualified:YES];
            [env updateImportedObject:val forKey:key];
            return val;
        }
    }
    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return val;
}

- (id<JSDataProtocol>)resolveExportFault:(JSFault *)fault forKey:(JSSymbol *)key inEnv:(Env *)env {
    JSSymbol *sym = [key copy];
    [sym setModuleName:[fault moduleName]];
    [sym setInitialModuleName:[fault moduleName]];
    id<JSDataProtocol> val = [self objectForExportedSymbol:sym module:[fault moduleName]];
    if (val) {  // update object in export table
        [sym setIsFault:NO];
        [sym setIsQualified:YES];
        [env updateExportedObject:val forKey:sym];
        return val;
    }
    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return val;
}

- (id<JSDataProtocol>)resolveFault:(id<JSDataProtocol>)object forKey:(JSSymbol *)key inEnv:(Env *)env {
    if ([JSFault isFault:object]) {
        JSFault *fault = (JSFault *)object;
        if ([fault isImported]) {
            [self resolveExportFault:fault forKey:key inEnv:[Env forModuleName:[fault moduleName]]];
            object = [self resolveImportFault:fault forKey:key inEnv:env];
        } else {  // Exported symbol
            object = [self resolveExportFault:fault forKey:key inEnv:[Env forModuleName:[fault moduleName]]];
        }
    }
    return object;
}

- (id<JSDataProtocol>)objectForKey:(JSSymbol *)key {
    return [self objectForKey:key isThrow:YES];
}

- (id<JSDataProtocol>)objectForKey:(JSSymbol *)key isThrow:(BOOL)isThrow {
    id<JSDataProtocol> elem = [self resolveFault:[self objectForSymbol:key isThrow:isThrow] forKey:key inEnv:self];
    if (!elem && isThrow) {
        if ([key isQualified]) [key setModuleName:[key initialModuleName]];
        [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    }
    return elem;
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key isThrow:(BOOL)isThrow {
    if ([key isQualified]) return [self objectForSymbol:key inModule:[Env forModuleName:[key initialModuleName]]];
    NSString *moduleName = [key moduleName];
    id<JSDataProtocol> elem = nil;
    if ([moduleName isEqual:_moduleName]) {
        if (_isExportAll || [moduleName isEqual:[Const defaultModuleName]] || [moduleName isEqual:[Const coreModuleName]]) {
            elem = [self objectForKeyFromExportTable:key];
            if (elem) return elem;
        } else {
            elem = [self objectForKeyFromInternalTable:key];
            if (elem) return elem;
        }
        elem = [self objectForKeyFromImportTable:key];
        if (elem) return elem;
    } else {
        // Symbol belongs to another module
        elem = [self objectForSymbol:key inModule:[Env forModuleName:[key moduleName]]];
        if (elem) return elem;
    }
    // Symbol may belong to core
    [key setModuleName:[Const coreModuleName]];
    elem = [self objectForSymbol:key inModule:[Env forModuleName:[Const coreModuleName]]];
    if (elem) return elem;
    [key resetModuleName];
    if (isThrow) [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return nil;
}

- (id<JSDataProtocol> _Nullable)objectForKeyFromImportTable:(JSSymbol *)key {
    id<JSDataProtocol> elem = [_importTable objectForSymbol:key];
    if (elem) return elem;
    return [self outer] ? [[self outer] objectForKeyFromImportTable:key] : nil;
}

- (id<JSDataProtocol> _Nullable)objectForKeyFromExportTable:(JSSymbol *)key {
    id<JSDataProtocol> elem = [_exportTable objectForSymbol:key];
    if (elem) return elem;
    return [self outer] ? [[self outer] objectForKeyFromExportTable:key] : nil;
}

- (id<JSDataProtocol> _Nullable)objectForKeyFromInternalTable:(JSSymbol *)key {
    id<JSDataProtocol> elem = [_internalTable objectForSymbol:key];
    if (elem) return elem;
    return [self outer] ? [[self outer] objectForKeyFromInternalTable:key] : nil;
}

- (id<JSDataProtocol> _Nullable)objectForSymbol:(JSSymbol *)key inModule:(Env *)env {
    JSSymbol *sym = [key copy];
    id<JSDataProtocol> elem;
    NSString *modName = [env moduleName];
    if ([key isQualified]) {
        [sym setModuleName:[key initialModuleName]];
    }
    if ([env isExportAll] || [modName isEqual:[Const defaultModuleName]] || [modName isEqual:[Const coreModuleName]]) {
        elem = [[env exportTable] objectForSymbol:sym];
        if (elem) return elem;
    } else {
        if ([[State currentModuleName] isEqual:modName]) {
            elem = [[env internalTable] objectForSymbol:sym];
            if (elem) return elem;
        } else {
            elem = [[env exportTable] objectForSymbol:sym];
            if (elem) return elem;
        }
    }
    if ([[State currentModuleName] isEqual:modName]) {
        elem = [[env importTable] objectForSymbol:key];
        if (elem) return elem;
    }
    return [env outer] ? [self objectForSymbol:key inModule:[env outer]] : nil;
}

/** Get object for exported symbol from the module. Used only for resolving export fault. */
- (id<JSDataProtocol> _Nullable)objectForExportedSymbol:(JSSymbol *)key module:(NSString *)name {
    Env *env = [Env forModuleName:name];
    JSSymbol *sym = [key copy];
    if (env) {
        [sym setModuleName:[key initialModuleName]];
        id<JSDataProtocol> elem;
        if ([env isExportAll]) {
            elem = [[env exportTable] objectForSymbol:sym];
            if (elem) return elem;
        } else {
            elem = [[env internalTable] objectForSymbol:sym];
            if (elem) return elem;
        }
        return [env outer] ? [[env outer] objectForKey:key isThrow:NO] : nil;
    }
    return nil;
}

- (void)setObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    [obj setModuleName:[State currentModuleName]];
    if (_isExportAll || [_moduleName isEqual:[Const defaultModuleName]] || [_moduleName isEqual:[Const coreModuleName]]) {
        [_exportTable setObject:obj forKey:key];
    } else {
        [_internalTable setObject:obj forKey:key];
    }
}

/** Update an existing key value pair with new properties for an imported symbol. Used only for resolving import fault. */
- (void)updateImportedObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    if (_isExportAll || [_moduleName isEqual:[Const defaultModuleName]] || [_moduleName isEqual:[Const coreModuleName]]) {
        [_exportTable updateObject:obj forKey:key];
    } else {
        [_internalTable updateObject:obj forKey:key];
    }
}

/** Update an existing key value pair with new properties for an exported symbol. Used only for resolving export fault. */
- (void)updateExportedObject:(id<JSDataProtocol>)obj forKey:(JSSymbol *)key {
    [_exportTable updateObject:obj forKey:key];
}

- (NSArray *)exportedFunctions {
    return [self exportedFunctions:[NSMutableArray new]];
}

- (NSArray *)exportedFunctions:(NSMutableArray *)acc {
    [acc addObjectsFromArray:[[[self exportTable] allKeys] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.arity > -2"]]];
    if (![self outer]) return acc;
    return [[self outer] exportedFunctions:acc];
}

- (NSArray *)importedFunctions {
    return [self importedFunctions:[NSMutableArray new]];
}

- (NSArray *)importedFunctions:(NSMutableArray *)acc {
    [acc addObjectsFromArray:[[[self importTable] allKeys] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.arity > -2"]]];
    if (![self outer]) return acc;
    return [[self outer] importedFunctions:acc];
}

- (NSArray *)internalFunctions {
    return [self internalFunctions:[NSMutableArray new]];
}

- (NSArray *)internalFunctions:(NSMutableArray *)acc {
    [acc addObjectsFromArray:[[[self internalTable] allKeys] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.arity > -2"]]];
    if (![self outer]) return acc;
    return [[self outer] internalFunctions:acc];
}


- (NSString *)description {
    return [NSString stringWithFormat:@"<%@ %p moduleName: %@, isExportAll: %hhd>", NSStringFromClass([self class]), self, [self moduleName], _isExportAll];
}

@end
