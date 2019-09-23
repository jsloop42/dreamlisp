//
//  DLSymbol.m
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLSymbol.h"

@implementation DLSymbol {
    NSString *_name;
    id<DLDataProtocol> _meta;
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
    BOOL _isMutable;
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
@synthesize isMutable = _isMutable;

+ (BOOL)isSymbol:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isSymbol:(id)object withName:(NSString *)name {
    return [self isSymbol:object] ? [[(DLSymbol *)object value] isEqual:name] : NO;
}

+ (DLSymbol *)dataToSymbol:(id<DLDataProtocol>)data fnName:(NSString *)fnName {
    return [self dataToSymbol:data position:-1 fnName:fnName];
}

+ (DLSymbol *)dataToSymbol:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName {
    if (![DLSymbol isSymbol:data]) {
        DLError *err = nil;
        if (position > 0) {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'symbol'", position, [data dataTypeName]];
        } else {
            err = [[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'symbol'", [data dataTypeName]];
        }
        [err throw];
    }
    return (DLSymbol *)data;
}

/**
 Returns a symbol from the given name. If symbol is of the form MFA, then the module and arity is updated accordingly. Works according to the reader logic
 for module. If qualified, the initial module is set to that module and module name is set to current one. */
+ (DLSymbol *)processName:(NSString *)name {
    DLSymbol *sym = nil;
    NSArray *modArr = [name componentsSeparatedByString:@":"];
    NSUInteger modCount = [modArr count];
    NSString *symName = name;
    if (modCount > 2) [[[DLError alloc] initWithFormat:DLSymbolParseError, name] throw];
    if (modCount == 2) symName = modArr[1];  // the function part
    NSArray *symArr = [symName componentsSeparatedByString:@"/"];
    NSUInteger count = [symArr count];
    NSString *arityStr = nil;
    NSInteger arity = -2;
    if (count == 3 && [symArr[1] isEmpty]) {  // (core://n 4 2)
        arityStr = symArr[2];
        symName = @"/";
    } else if (count > 2) {
        [[[DLError alloc] initWithFormat:DLSymbolParseError, name] throw];
    } else if (count == 2) {
        symName = symArr[0];
        arityStr = symArr[1];
    }
    if ([arityStr isNotEmpty]) {
        arity = [arityStr isEqual:@"n"] ? -1 : [arityStr integerValue];
    }
    sym = [[DLSymbol alloc] initWithName:symName moduleName:[DLState.shared currentModuleName]];
    [sym setInitialArity:arity];
    [sym resetArity];
    // Fully qualified symbol with module name included
    if (modCount == 2) {
        [sym setIsQualified:YES];
        [sym setInitialModuleName:modArr[0]];
    }
    return sym;
}

/**
  Update the symbol with function info and function with the bounded symbol info.

  @param symbol A symbol.
  @param object The object bound to the symbol. The object will be updated in place if it is a function.
  @return symbol The symbol with updates if any
 */
+ (DLSymbol *)symbolWithArityCheck:(DLSymbol *)symbol withObject:(id)object {
    if ([DLFunction isFunction:object]) {
        DLFunction *fn = (DLFunction *)object;
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

/** Order symbol in the decreasing order of arity. -1 which is n arity, n - 1, .. 2, 1, 0, -2, which means no arity (not a function).  */
+ (NSComparisonResult)compareSymbol:(DLSymbol *)aSymbol withSymbol:(DLSymbol *)bSymbol {
    NSInteger aSymArity = [aSymbol arity];
    NSInteger bSymArity = [bSymbol arity];
    if (aSymArity == -2 || bSymArity == -1) return NSOrderedDescending;
    if (aSymArity == -1 || bSymArity == -2) return NSOrderedAscending;
    if (aSymArity > bSymArity) return NSOrderedAscending;
    if (aSymArity < bSymArity) return NSOrderedDescending;
    return NSOrderedSame;
}

- (void)dealloc {
    [DLLog debug:@"DLSymbol dealloc"];
}

- (instancetype)initWithName:(NSString *)name {
    self = [self initWithName:name moduleName:_moduleName];
    _moduleName = [DLState.shared currentModuleName];
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
- (instancetype)initWithFunction:(DLFunction *)func name:(NSString *)name moduleName:(NSString *)moduleName {
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

- (instancetype)initWithArity:(NSInteger)arity symbol:(DLSymbol *)symbol {
    return [self initWithArity:arity position:-1 symbol:symbol];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position symbol:(DLSymbol *)symbol {
    if (arity < -1) [[[DLError alloc] initWithDescription:DLFunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = [symbol name];
        _meta = [symbol meta];
        _initialArity = arity;
        _arity = arity;
        _position = position;
        _hasNArity = [symbol hasNArity];
        _moduleName = [symbol moduleName] ? [symbol moduleName] : [DLState.shared currentModuleName];
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
    return [self initWithArity:arity position:position string:string moduleName:[DLState.shared currentModuleName]];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string moduleName:(NSString *)moduleName {
    if (arity < -1) [[[DLError alloc] initWithDescription:DLFunctionArityError] throw];
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

- (instancetype)initWithMeta:(id<DLDataProtocol>)meta symbol:(DLSymbol *)symbol {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = [symbol name];
        _isFunction = [symbol isFunction];
        _hasNArity = [symbol hasNArity];
        _position = [symbol position];
        _meta = meta;
        _moduleName = [symbol moduleName] ? [symbol moduleName] : [DLState.shared currentModuleName];
        _initialModuleName = [symbol initialModuleName];
        _isQualified = [symbol isQualified];
        _isImported = [symbol isImported];
    }
    return self;
}

- (instancetype)initWithMeta:(_Nullable id<DLDataProtocol>)meta name:(NSString *)name {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
        _meta = meta;
        _moduleName = [DLState.shared currentModuleName];
        _initialModuleName = _moduleName;
        _isQualified = NO;
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _name = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_value"];
        _arity = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_arity"] integerValue];
        _initialArity = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_initialArity"] integerValue];
        NSValue *isFunction = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isFunction"];
        [isFunction getValue:&_isFunction];
        NSValue *hasNArityValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_hasNArity"];
        [hasNArityValue getValue:&_hasNArity];
        NSValue *isQualifiedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isQualified"];
        [isQualifiedValue getValue:&_isQualified];
        NSValue *isModuleValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isModule"];
        [isModuleValue getValue:&_isModule];
        NSValue *isFaultValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isFault"];
        [isFaultValue getValue:&_isFault];
        NSValue *isCoreValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isCore"];
        [isCoreValue getValue:&_isCore];
        _fnName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_fnName"];
        _initialModuleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_initialModuleName"];
        _meta = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_meta"];
        _moduleName = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_moduleName"];
        _position = [[coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_position"] integerValue];
        NSValue *isImportedValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isImported"];
        [isImportedValue getValue:&_isImported];
        NSValue *isMutableValue = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLSymbol_isMutable"];
        [isMutableValue getValue:&_isMutable];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_name forKey:@"DLSymbol_value"];
    [coder encodeObject:@(_arity) forKey:@"DLSymbol_arity"];
    [coder encodeObject:@(_initialArity) forKey:@"DLSymbol_initialArity"];
    [coder encodeObject:_fnName forKey:@"DLSymbol_fnName"];
    [coder encodeObject:_initialModuleName forKey:@"DLSymbol_initialModuleName"];
    NSValue *isFunctionValue = [[NSValue alloc] initWithBytes:&_isFunction objCType:@encode(BOOL)];
    [coder encodeObject:isFunctionValue forKey:@"DLSymbol_isFunction"];
    NSValue *hasNArityValue = [[NSValue alloc] initWithBytes:&_hasNArity objCType:@encode(BOOL)];
    [coder encodeObject:hasNArityValue forKey:@"DLSymbol_hasNArity"];
    NSValue *isQualifiedValue = [[NSValue alloc] initWithBytes:&_isQualified objCType:@encode(BOOL)];
    [coder encodeObject:isQualifiedValue forKey:@"DLSymbol_isQualified"];
    NSValue *isModule = [[NSValue alloc] initWithBytes:&_isModule objCType:@encode(BOOL)];
    [coder encodeObject:isModule forKey:@"DLSymbol_isModule"];
    NSValue *isFault = [[NSValue alloc] initWithBytes:&_isFault objCType:@encode(BOOL)];
    [coder encodeObject:isFault forKey:@"DLSymbol_isFault"];
    NSValue *isCore = [[NSValue alloc] initWithBytes:&_isCore objCType:@encode(BOOL)];
    [coder encodeObject:isCore forKey:@"DLSymbol_isCore"];
    [coder encodeObject:_meta forKey:@"DLSymbol_meta"];
    [coder encodeObject:_moduleName forKey:@"DLSymbol_moduleName"];
    [coder encodeObject:@(_position) forKey:@"DLSymbol_position"];
    NSValue *isImportedValue = [[NSValue alloc] initWithBytes:&_isImported objCType:@encode(BOOL)];
    [coder encodeObject:isImportedValue forKey:@"DLSymbol_isImported"];
    NSValue *isMutableValue = [[NSValue alloc] initWithBytes:&_isMutable objCType:@encode(BOOL)];
    [coder encodeObject:isMutableValue forKey:@"DLSymbol_isMutable"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

- (void)bootstrap {
    _position = -1;
    _moduleName = [DLState.shared currentModuleName];
    _initialModuleName = _moduleName;
    _isQualified = NO;
    _fnName = @"";
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

- (id<DLDataProtocol>)setPosition:(NSInteger)position {
    _position = position;
    return self;
}

- (NSString *)name {
    return _name;
}

- (DLSymbol *)toNArity {
    _arity = -1;
    [self updateArity];
    return self;
}

- (DLSymbol *)resetArity {
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
                                                                                                             initWithFormat:@"%ld", _initialArity]];
}

- (void)copyMeta:(id<DLDataProtocol>)object {
    _meta = [object meta];
}

- (void)copyProperties:(DLSymbol *)symbol {
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

- (BOOL)isEqual:(id)object {
    if (![DLSymbol isSymbol:object]) return NO;
    DLSymbol *sym = (DLSymbol *)object;
    return [_name isEqual:[sym name]] && _arity == [sym arity] && [_moduleName isEqual:[sym moduleName]];
}

- (NSUInteger)hash {
    return [_moduleName hash] + [_name hash] + _arity + 2;  // Adding 2 to offset negative arity.
}

- (NSInteger)sortValue {
    return [self hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    DLSymbol *elem = [[DLSymbol alloc] initWithName:_name];
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
    return _name;
}

- (NSString *)debugDescription {
    return [[NSString alloc] initWithFormat:@"<%@ %p - ModuleName: %@, InitialModuleName: %@, " \
            @"Name: %@, Arity:%ld, InitialArity: %ld, " \
            @"isQualified: %hhd, isFault: %hhd, " \
            @"meta: %@>",
            NSStringFromClass([self class]), self, _moduleName, _initialModuleName, _name, _arity, _initialArity, _isQualified, _isFault, _meta];
}

@end
