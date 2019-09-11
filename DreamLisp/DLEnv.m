//
//  DLEnv.m
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLEnv.h"

/** Holds the module envs loaded. */
static NSMapTable<NSString *, DLEnv *> *_modules;

/** An env associated with a module. There is a global env and one specific for each module. */
@implementation DLEnv {
    DLEnv *_outer;
    /** Exported symbols for the module. If no module is defined, then the symbols are global. */
    DLModuleTable *_exportTable;
    /** Imported symbols for the module. */
    DLModuleTable *_importTable;
    /** The internal env table containing evaluated symbols with its binding. */
    DLModuleTable *_internalTable;
    /** The symbol table associated with the module. */
    DLSymbolTable *_symbolTable;
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
@synthesize symbolTable = _symbolTable;
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
+ (void)setEnv:(DLEnv *)env forModuleName:(NSString *)moduleName {
    [_modules setObject:env forKey:moduleName];
}

/** Returns env for the given module name. */
+ (DLEnv *)envForModuleName:(NSString *)moduleName {
    return [_modules objectForKey:moduleName];
}

/** Removes env for the given module name. */
+ (void)removeModule:(NSString *)moduleName {
    [_modules removeObjectForKey:moduleName];
}

/** Returns the internal modules table. */
+ (NSMapTable<NSString *, DLEnv *> *)modules {
    return _modules;
}

#pragma mark Env

- (instancetype)initWithEnv:(DLEnv *)env {
    self = [super init];
    if (self) {
        [self bootstrap];
        _moduleName = [env moduleName];
        _isExportAll = [env isExportAll];
        _isUserDefined = [env isUserDefined];
        _symbolTable = [env symbolTable];
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

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _outer = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_outer"];
        _exportTable = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_exportTable"];
        _importTable = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_importTable"];
        _internalTable = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_internalTable"];
        _symbolTable = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_symbolTable"];
        NSValue *isUserDefinedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_isUserDefined"];
        [isUserDefinedValue getValue:&_isUserDefined];
        NSValue *isExportAllValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"Env_isExportAll"];
        [isExportAllValue getValue:&_isExportAll];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_outer forKey:@"Env_outer"];
    [coder encodeObject:_exportTable forKey:@"Env_exportTable"];
    [coder encodeObject:_importTable forKey:@"Env_importTable"];
    [coder encodeObject:_internalTable forKey:@"Env_internalTable"];
    [coder encodeObject:_symbolTable forKey:@"Env_symbolTable"];
    [coder encodeObject:_moduleName forKey:@"Env_moduleName"];
    [coder encodeObject:_moduleDescription forKey:@"Env_moduleDescription"];
    NSValue *isUserDefinedValue = [[NSValue alloc] initWithBytes:&_isUserDefined objCType:@encode(BOOL)];
    [coder encodeObject:isUserDefinedValue forKey:@"Env_isUserDefined"];
    NSValue *isExportAllValue = [[NSValue alloc] initWithBytes:&_isExportAll objCType:@encode(BOOL)];
    [coder encodeObject:isExportAllValue forKey:@"Env_isExportAll"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

/** If the symbol is associated with a function, update the symbol with function details. */
- (void)setFunctionInfo:(id<DLDataProtocol>)object symbol:(DLSymbol *)symbol {
    if ([DLFunction isFunction:object]) {
        [symbol setIsFunction:YES];
        [symbol setArity:[(DLFunction *)object argsCount]];
    }
    //[symbol setModuleName:[_module name]];
//    return symbol;
}

/**
 Initializes environment with an outer environment and binds symbols with expressions. The current env is used instead of the symbol's env. Symbols can be
 qualified which refers to other env.

 @param env The outer environment.
 @param binds A array of `DLSymbol` symbols.
 @param exprs A array of `id<DLDataProtocol>` expressions.
 */
- (instancetype)initWithEnv:(DLEnv *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs {
    self = [super init];
    NSUInteger len = [binds count];
    NSUInteger i = 0;
    if (self) {
        [self bootstrap];
        _moduleName = [env moduleName];
        _isExportAll = [env isExportAll];
        _outer = env;
        DLSymbol *sym = nil;
        DLSymbol *key = nil;
        DLList *list = nil;
        for (i = 0; i < len; i++) {
            sym = (DLSymbol *)binds[i];
            if ([[sym value] isEqual:@"&"]) {
                key = (DLSymbol *)binds[i + 1];
                if ([exprs count] > i) {
                    [self setObject:[[DLList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:key];
                    [_symbolTable setKey:key];
                } else {
                    list = [[DLList alloc] initWithArray:@[]];
                    [self setFunctionInfo:list symbol:key];
                    [self setObject:list forKey:key];
                    [_symbolTable setKey:key];
                }
                break;
            }
            [self setFunctionInfo:exprs[i] symbol:sym];
            [self setObject:exprs[i] forKey:sym];
            [_symbolTable setKey:sym];
        }
    }
    return self;
}

- (void)bootstrap {
    if (!_exportTable) _exportTable = [DLModuleTable new];
    if (!_importTable) _importTable = [DLModuleTable new];
    if (!_internalTable) _internalTable = [DLModuleTable new];
    if (!_symbolTable) _symbolTable = [DLSymbolTable new];
    _moduleName = DLConst.defaultModuleName;
    _moduleDescription = @"";
    _isExportAll = NO;
    _isUserDefined = YES;
}

- (id<DLDataProtocol>)resolveImportFault:(DLFault *)fault forKey:(DLSymbol *)key inEnv:(DLEnv *)env {
    id<DLDataProtocol> val = nil;
    DLEnv *modEnv = [DLEnv envForModuleName:[fault moduleName]];
    DLSymbol *sym = [key copy];
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
    [[[DLError alloc] initWithFormat:DLSymbolNotFound, [key string]] throw];
    return val;
}

- (id<DLDataProtocol>)resolveExportFault:(DLFault *)fault forKey:(DLSymbol *)key inEnv:(DLEnv *)env {
    DLSymbol *sym = [key copy];
    [sym setModuleName:[fault moduleName]];
    [sym setInitialModuleName:[fault moduleName]];
    id<DLDataProtocol> val = [self objectForExportedSymbol:sym module:[fault moduleName]];
    if (val) {  // update object in export table
        [sym setIsFault:NO];
        [sym setIsQualified:YES];
        [env updateExportedObject:val forKey:sym];
        return val;
    }
    [[[DLError alloc] initWithFormat:DLSymbolNotFound, [key string]] throw];
    return val;
}

- (id<DLDataProtocol>)resolveFault:(id<DLDataProtocol>)object forKey:(DLSymbol *)key inEnv:(DLEnv *)env {
    if ([DLFault isFault:object]) {
        DLFault *fault = (DLFault *)object;
        if ([fault isImported]) {
            [self resolveExportFault:fault forKey:key inEnv:[DLEnv envForModuleName:[fault moduleName]]];
            object = [self resolveImportFault:fault forKey:key inEnv:env];
        } else {  // Exported symbol
            object = [self resolveExportFault:fault forKey:key inEnv:[DLEnv envForModuleName:[fault moduleName]]];
        }
    }
    return object;
}

- (id<DLDataProtocol> _Nullable)objectForKey:(DLSymbol *)key {
    return [self objectForKey:key isThrow:YES isFromSymbolTable:NO];
}

- (id<DLDataProtocol> _Nullable)objectForKey:(DLSymbol *)key isThrow:(BOOL)isThrow {
    return [self objectForKey:key isThrow:isThrow isFromSymbolTable:NO];
}

- (id<DLDataProtocol> _Nullable)objectForKey:(DLSymbol *)key isThrow:(BOOL)isThrow isFromSymbolTable:(BOOL)isFromSymbolTable {
    id<DLDataProtocol> elem = [self resolveFault:[self objectForSymbol:key isThrow:isThrow isFromSymbolTable:isFromSymbolTable] forKey:key inEnv:self];
    if (!elem && isThrow) {
        if ([key isQualified]) [key setModuleName:[key initialModuleName]];
        [[[DLError alloc] initWithFormat:DLSymbolNotFound, [key string]] throw];
    }
    return elem;
}

- (id<DLDataProtocol>)objectForSymbol:(DLSymbol *)key isThrow:(BOOL)isThrow {
    return [self objectForSymbol:key isThrow:isThrow isFromSymbolTable:NO];
}

- (id<DLDataProtocol>)objectForSymbol:(DLSymbol *)key isThrow:(BOOL)isThrow isFromSymbolTable:(BOOL)isFromSymbolTable {
    DLSymbol *sym = [self symbolForKeyFromSymbolTable:key];  // Check the symbol table first to find any local scope bindings for the given symbol.
    if (sym) key = sym;
    if ([key isQualified]) return [self objectForSymbol:key inModule:[DLEnv envForModuleName:[key initialModuleName]]];
    NSString *moduleName = [key moduleName];
    id<DLDataProtocol> elem = nil;
    if ([moduleName isEqual:_moduleName]) {
        if (_isExportAll || [moduleName isEqual:DLConst.defaultModuleName] || [moduleName isEqual:DLConst.coreModuleName]) {
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
        elem = [self objectForSymbol:key inModule:[DLEnv envForModuleName:[key moduleName]]];
        if (elem) return elem;
    }
    // Symbol may belong to core
    [key setModuleName:[DLConst coreModuleName]];
    elem = [self objectForSymbol:key inModule:[DLEnv envForModuleName:[DLConst coreModuleName]]];
    if (elem) return elem;
    [key resetModuleName];
    if (isThrow) [[[DLError alloc] initWithFormat:DLSymbolNotFound, [key string]] throw];
    return nil;
}

- (id<DLDataProtocol> _Nullable)objectForKeyFromImportTable:(DLSymbol *)key {
    id<DLDataProtocol> elem = [_importTable objectForSymbol:key];
    if (elem) return elem;
    return [self outer] ? [[self outer] objectForKeyFromImportTable:key] : nil;
}

- (id<DLDataProtocol> _Nullable)objectForKeyFromExportTable:(DLSymbol *)key {
    id<DLDataProtocol> elem = [_exportTable objectForSymbol:key];
    if (elem) return elem;
    return [self outer] ? [[self outer] objectForKeyFromExportTable:key] : nil;
}

- (id<DLDataProtocol> _Nullable)objectForKeyFromInternalTable:(DLSymbol *)key {
    id<DLDataProtocol> elem = [_internalTable objectForSymbol:key];
    if (elem) return elem;
    return [self outer] ? [[self outer] objectForKeyFromInternalTable:key] : nil;
}

- (DLSymbol * _Nullable)symbolForKeyFromSymbolTable:(DLSymbol *)key {
    DLSymbol *sym = [_symbolTable symbolForKey:key];
    if (sym) return sym;
    return [self outer] ? [[self outer] symbolForKeyFromSymbolTable:key] : nil;
}


- (id<DLDataProtocol> _Nullable)objectForSymbol:(DLSymbol *)key inModule:(DLEnv *)env {
    DLSymbol *sym = [key copy];
    id<DLDataProtocol> elem;
    NSString *modName = [env moduleName];
    if ([key isQualified]) {
        [sym setModuleName:[key initialModuleName]];
    }
    if ([env isExportAll] || [modName isEqual:DLConst.defaultModuleName] || [modName isEqual:DLConst.coreModuleName]) {
        elem = [[env exportTable] objectForSymbol:sym];
        if (elem) return elem;
    } else {
        if ([[DLState currentModuleName] isEqual:modName]) {
            elem = [[env internalTable] objectForSymbol:sym];
            if (elem) return elem;
        } else {
            elem = [[env exportTable] objectForSymbol:sym];
            if (elem) return elem;
        }
    }
    if ([[DLState currentModuleName] isEqual:modName]) {
        elem = [[env importTable] objectForSymbol:key];
        if (elem) return elem;
    }
    return [env outer] ? [self objectForSymbol:key inModule:[env outer]] : nil;
}

/** Get object for exported symbol from the module. Used only for resolving export fault. */
- (id<DLDataProtocol> _Nullable)objectForExportedSymbol:(DLSymbol *)key module:(NSString *)name {
    DLEnv *env = [DLEnv envForModuleName:name];
    DLSymbol *sym = [key copy];
    if (env) {
        [sym setModuleName:[key initialModuleName]];
        id<DLDataProtocol> elem;
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

- (void)setObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key {
    [obj setModuleName:[DLState currentModuleName]];
    if (_isExportAll || [_moduleName isEqual:DLConst.defaultModuleName] || [_moduleName isEqual:DLConst.coreModuleName]) {
        [_exportTable setObject:obj forKey:key];
    } else {
        [_internalTable setObject:obj forKey:key];
    }
    [_symbolTable setKey:key];
}

/** Update an existing key value pair with new properties for an imported symbol. Used only for resolving import fault. */
- (void)updateImportedObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key {
    if (_isExportAll || [_moduleName isEqual:DLConst.defaultModuleName] || [_moduleName isEqual:DLConst.coreModuleName]) {
        [_exportTable updateObject:obj forKey:key];
    } else {
        [_internalTable updateObject:obj forKey:key];
    }
    [_symbolTable setKey:key];
}

/** Update an existing key value pair with new properties for an exported symbol. Used only for resolving export fault. */
- (void)updateExportedObject:(id<DLDataProtocol>)obj forKey:(DLSymbol *)key {
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
