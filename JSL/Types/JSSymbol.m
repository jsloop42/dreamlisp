//
//  JSSymbol.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSSymbol.h"

@implementation JSSymbol {
    NSString *_name;
    id<JSDataProtocol> _meta;
    NSInteger _arity;
    NSInteger _initialArity;
    BOOL _isFunction;
    BOOL _hasNArity;
    NSInteger _position;
    /** Function name with arity if the symbol is bound to a function */
    NSString *_fnName;
    NSString *_moduleName;
    NSString *_initialModuleName;
    BOOL _isQualified;
    BOOL _isModule;
    BOOL _isFault;
    BOOL _isImported;
    BOOL _isCore;  // If it is a core symbol of the form def, defmacro, let, if etc.
}

@synthesize arity = _arity;
@synthesize initialArity = _initialArity;
@synthesize isFunction = _isFunction;
@synthesize hasNArity = _hasNArity;
@synthesize meta = _meta;
@synthesize value = _name;
@synthesize fnName = _fnName;
@synthesize moduleName = _moduleName;
@synthesize initialModuleName = _initialModuleName;
@synthesize isQualified = _isQualified;
@synthesize isModule = _isModule;
@synthesize isFault = _isFault;
@synthesize isImported = _isImported;
@synthesize isCore = _isCore;

+ (BOOL)isSymbol:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isSymbol:(id)object withName:(NSString *)name {
    return [self isSymbol:object] ? [[(JSSymbol *)object value] isEqual:name] : NO;
}

+ (JSSymbol *)dataToSymbol:(id<JSDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToSymbol:data position:-1 fnName:fnName];
}

+ (JSSymbol *)dataToSymbol:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![JSSymbol isSymbol:data]) {
        JSError *err = nil;
        if (position > 0) {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'symbol'", position, [data dataTypeName]];
        } else {
            err = [[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'symbol'", [data dataTypeName]];
        }
        [err throw];
    }
    return (JSSymbol *)data;
}

/** Returns a symbol from the given name. If symbol is of the form MFA, then the module and arity is updated accordingly. This */
+ (JSSymbol *)processName:(NSString *)name {
    NSArray *modArr = [name componentsSeparatedByString:@":"];
    NSUInteger modCount = [modArr count];
    if (modCount > 2) [[[JSError alloc] initWithFormat:SymbolParseError, name] throw];
    if (modCount == 2) name = modArr[1];  // the function part
    NSArray *symArr = [name componentsSeparatedByString:@"/"];
    NSUInteger count = [symArr count];
    NSString *arity = nil;
    JSSymbol *sym = nil;
    if (count > 2) [[[JSError alloc] initWithFormat:SymbolParseError, name] throw];
    if (count == 2) {
        arity = symArr[1];
        if ([arity isNotEmpty]) {
            sym = [[JSSymbol alloc] initWithArity:[arity isEqual:@"n"] ? -1 : [arity integerValue] string:symArr[0]];
        } else {
            [[[JSError alloc] initWithFormat:SymbolParseError, name] throw];
        }
    } else if (count == 1) {
        sym = [[JSSymbol alloc] initWithName:name];
    }
    // Fully qualified symbol with module name included
    if (modCount == 2) {
        [sym setIsQualified:YES];
        [sym setInitialModuleName:modArr[0]];
    }
    [sym setModuleName:[State currentModuleName]];
    return sym;
}

/**
  Update the symbol with function info and function with the bounded symbol info.

  @param symbol A symbol.
  @param object The object bound to the symbol. The object will be updated in place if it is a function.
  @return symbol The symbol with updates if any
 */
+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object {
    if ([JSFunction isFunction:object]) {
        JSFunction *fn = (JSFunction *)object;
        [symbol setIsFunction:YES];
        [symbol setInitialArity:[fn argsCount]];
        [symbol resetArity];
        if (![fn moduleName] || [[fn moduleName] isEmpty]) {
            [fn setModuleName:[symbol moduleName]];
        }
        [symbol setIsImported:[object isImported]];
        [fn setName:[symbol string]];
    }
    return symbol;
}

- (instancetype)initWithName:(NSString *)name {
    self = [self initWithName:name moduleName:_moduleName];
    _moduleName = [State currentModuleName];
    _initialModuleName = _moduleName;
    return self;
}

- (instancetype)initWithName:(NSString *)name moduleName:(NSString *)moduleName {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
        _arity = -2;
        _initialArity = -2;
        _moduleName = moduleName;
        _initialModuleName = moduleName;
        [self updateArity];
    }
    return self;
}

/** Initialise a symbol with function details and module name. */
- (instancetype)initWithFunction:(JSFunction *)func name:(NSString *)name moduleName:(NSString *)moduleName {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
        _initialArity = [func argsCount];
        _moduleName = moduleName;
        _initialModuleName = moduleName;
        _isQualified = YES;
        _position = 0;
        [self resetArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity symbol:(JSSymbol *)symbol {
    return [self initWithArity:arity position:-1 symbol:symbol];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position symbol:(JSSymbol *)symbol {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = [symbol name];
        _meta = [symbol meta];
        _initialArity = arity;
        _arity = arity;
        _position = position;
        _hasNArity = [symbol hasNArity];
        _moduleName = [symbol moduleName] ? [symbol moduleName] : [State currentModuleName];
        _initialModuleName = [symbol initialModuleName];
        _isQualified = [symbol isQualified];
        _isImported = [symbol isImported];
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string {
    return [self initWithArity:arity position:-1 string:string];
}

- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string moduleName:(NSString *)moduleName {
    self = [self initWithArity:arity position:-1 string:string];
    _moduleName = moduleName;
    _initialModuleName = moduleName;
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string {
    return [self initWithArity:arity position:position string:string moduleName:[State currentModuleName]];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string moduleName:(NSString *)moduleName {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = string;
        _arity = arity;
        _initialArity = arity;
        _position = position;
        _moduleName = moduleName;
        _initialModuleName = _moduleName;
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = [symbol name];
        _isFunction = [symbol isFunction];
        _hasNArity = [symbol hasNArity];
        _position = [symbol position];
        _meta = meta;
        _moduleName = [symbol moduleName] ? [symbol moduleName] : [State currentModuleName];
        _initialModuleName = [symbol initialModuleName];
        _isQualified = [symbol isQualified];
        _isImported = [symbol isImported];
    }
    return self;
}

- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
        _meta = meta;
        _moduleName = [State currentModuleName];
        _initialModuleName = _moduleName;
        _isQualified = NO;
        [self updateArity];
    }
    return self;
}

- (void)bootstrap {
    _position = -1;
    _moduleName = [State currentModuleName];
    _initialModuleName = _moduleName;
    _isQualified = NO;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"symbol";
}

- (NSInteger)position {
    return _position;
}

- (id<JSDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSString *)name {
    return _name;
}

- (JSSymbol *)toNArity {
    _arity = -1;
    [self updateArity];
    return self;
}

- (JSSymbol *)resetArity {
    _arity = _initialArity;
    [self updateArity];
    return self;
}

- (void)updateArity {
    _hasNArity = _arity == -1;
    _isFunction = _arity >= -1;
}

- (void)resetModuleName {
    _moduleName = _initialModuleName;
}

- (NSString *)string {
    if (_initialArity <= -2) {
        if (_isModule) return _name;
        if (_position == 0 && _isQualified) return [[NSString alloc] initWithFormat:@"%@:%@", _initialModuleName, _name];  // => function position
        return [[NSString alloc] initWithFormat:@"%@:%@", _moduleName, _name];
    }
    return [[NSString alloc] initWithFormat:@"%@:%@/%@", _moduleName, _name, (_initialArity == -1) ? @"n" : [[NSString alloc]
                                                                                                             initWithFormat:@"%ld", _initialArity]];    return @"";
}

- (void)copyProperties:(JSSymbol *)symbol {
    _initialArity = [symbol initialArity];
    _arity = _initialArity;
    _isFunction = [symbol isFunction];
    _meta = [symbol meta];
    _position = [symbol position];
    _moduleName = [symbol moduleName];
    _initialModuleName = [symbol initialModuleName];
    [self updateArity];
}

- (BOOL)isEqualToName:(NSString *)name {
    return [_name isEqual:name];
}

- (BOOL)isEqual:(id)symbol {
    if (![JSSymbol isSymbol:symbol]) return NO;
    return [_name isEqual:[symbol name]] && _arity == [symbol arity] && [_moduleName isEqual:[symbol moduleName]];
}

- (NSUInteger)hash {
    return [_moduleName hash] + [_name hash] + _arity + 2;  // Adding 2 to offset negative arity.
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    JSSymbol *elem = [[JSSymbol alloc] initWithName:_name];
    [elem setPosition:_position];
    [elem setArity:_arity];
    [elem setInitialArity:_initialArity];
    [elem setModuleName:_moduleName];
    [elem setInitialModuleName:_initialModuleName];
    [elem setIsFunction:_isFunction];
    [elem setFnName:_fnName];
    [elem setIsQualified:_isQualified];
    [elem setIsFault:_isFault];
    [elem setIsCore:_isCore];
    [elem setIsFunction:_isFunction];
    [elem setIsModule:_isModule];
    [elem setMeta:_meta];
    return elem;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [self copyWithZone:zone];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - ModuleName: %@, InitialModuleName: %@, " \
            @"Name: %@, Arity:%ld, InitialArity: %ld, " \
            @"isQualified: %hhd, isFault: %hhd, " \
            @"meta: %@>",
            NSStringFromClass([self class]), self, _moduleName, _initialModuleName, _name, _arity, _initialArity, _isQualified, _isFault, _meta];
}

@end
