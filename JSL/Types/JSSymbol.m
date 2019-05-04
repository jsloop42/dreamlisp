//
//  JSSymbol.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSSymbol.h"

NSArray *keywords = nil;

@implementation JSSymbol {
    NSString *_name;
    NSString *_initialName;
    id<JSDataProtocol> _meta;
    NSInteger _arity;
    NSInteger _initialArity;
    BOOL _isFunction;
    BOOL _hasNArity;
    NSInteger _position;
}

@synthesize arity = _arity;
@synthesize initialArity = _initialArity;
@synthesize isFunction = _isFunction;
@synthesize hasNArity = _hasNArity;
@synthesize meta = _meta;
@synthesize value = _name;
@synthesize initialValue = _initialName;

+ (void)initialize {
    if (self == [self class]) {
        keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"macroexpand", @"try*", @"catch*"];
    }
}

+ (BOOL)isSymbol:(id)object {
    return [[object className] isEqual:[self className]];
}

+ (BOOL)isSymbol:(id)object withName:(NSString *)name {
    return [self isSymbol:object] ? [[(JSSymbol *)object value] isEqual:name] : NO;
}

+ (JSSymbol *)symbolWithArityCheck:(JSSymbol *)symbol withObject:(id)object {
    return [JSFunction isFunction:object] ? [[JSSymbol alloc] initWithArity:[(JSFunction *)object argsCount] symbol:symbol] : symbol;
}

/** If the given symbol is present in the symbol table, updates the given symbol to match. */
+ (JSSymbol * _Nullable)updateSymbol:(JSSymbol *)symbol {
    JSSymbol *aSym = nil;
    if ((aSym = [self containsSymbol:symbol]) != nil) {
        [symbol setValue:[aSym value]];
        return symbol;
    }
    return nil;
}

+ (JSHashMap * _Nullable)updateBindingsForHashMap:(JSHashMap *)ast {
    NSArray *keys = [ast allKeys];
    NSUInteger len = [keys count];
    NSUInteger i = 0;
    id<JSDataProtocol> obj = nil;
    for (i = 0; i < len; i++) {
        //obj =
    }
    return ast;
}

+ (JSVector *)updateBindingsForVector:(JSVector *)ast {
    NSMutableArray<id<JSDataProtocol>> *arr = [ast value];
    NSLock *arrLock = [NSLock new];
    [arr enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id<JSDataProtocol>  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        JSSymbol *aSym = nil;
        if ([JSSymbol isSymbol:obj] && ((aSym = [self updateSymbol:obj]) != nil)) {
            [arrLock lock];
            [arr update:aSym atIndex:idx];
            [arrLock unlock];
        }
    }];
    [ast setValue:arr];
    return ast;
}

/** Matches and updates gensym symbols. */
+ (JSSymbol *)matchAndSet:(JSSymbol *)symbol {
    JSSymbol *match = nil;
    if ((match = [self containsSymbol:symbol]) != nil) {
        if ([symbol isGensym]) {
            [SymbolTable removeSymbol:match];
            [SymbolTable setSymbol:[symbol autoGensym]];
        }
    } else {
        // Skip symbols in function position if it is not matched by argument
        if ([symbol position] != 0) [SymbolTable setSymbol:[symbol autoGensym]];
    }
    return symbol;
}

+ (JSSymbol * _Nullable)containsSymbol:(JSSymbol *)symbol {
    return [SymbolTable symbol:symbol];
}

+ (JSList *)updateBindingsForAST:(JSList *)ast {
    if (![JSList isList:ast]) return ast;
    NSUInteger len = [ast count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        id<JSDataProtocol> elem = [ast nth:i];
        if ([JSSymbol isSymbol:elem]) {
            JSSymbol *sym = (JSSymbol *)elem;
            if ([sym position] == 0 && [sym isEqualToName:@"let*"]) {  // (let* exp)
                NSMutableArray *bindings = [(JSList *)[ast nth:i + 1] value]; // bindings -> list or vector
                NSUInteger j = 0;
                NSUInteger blen = [bindings count];
                // Check if any of the symbols are redefined
                id<JSDataProtocol> aSym = nil;
                for (j = 0; j < blen; j += 2) {
                    aSym = bindings[j];
                    if ([JSList isList:aSym]) {
                        aSym = [self updateBindingsForAST:aSym];
                    } else if ([JSSymbol isSymbol:aSym]) {
                        //[self matchAndSet:aSym];
                        JSSymbol *elem = (JSSymbol *)aSym;
                        [self updateSymbol:elem];  // let* binding symbols
                        if (![elem isGensym]) {
                            [elem autoGensym];
                            [SymbolTable setSymbol:elem];
                        }
                    }
                    id<JSDataProtocol> exp = [self updateBindingsForAST:bindings[j + 1]];
                    if ([JSSymbol isSymbol:exp withName:@"let*"]) {
                        exp = [self updateBindingsForAST:exp];
                    } else if ([JSList isList:exp]) {
                        exp = [self updateBindingsForAST:exp];
                    } else if ([JSVector isVector:exp]) {
                        exp = [self updateBindingsForVector:exp];
                    } else if ([JSHashMap isHashMap:exp]) {
                        exp = [self updateBindingsForHashMap:exp];
                    }
                }
                i++;
                continue;
            } else if ([sym position] == 0 && ([sym isEqualToName:@"def!"] || [sym isEqualToName:@"defmacro!"])) {
                i++;
                [SymbolTable setSymbol:[ast nth:i]];  // def! binding name
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"fn*"]) {
                i++;
                JSList* elem = [ast nth:i];  // fn arguments
                NSMutableArray *arr = [elem value];
                NSMutableArray *symArgs = [NSMutableArray new];
                NSLock *lock = [NSLock new];
                NSInteger __block ampIndex = -1;  // & index
                [arr enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                    id<JSDataProtocol> arg = arr[idx];
                    if ([JSSymbol isSymbol:arg]) {
                        JSSymbol *aSym = (JSSymbol *)arg;
                        if ([[aSym value] isEqual:@"&"]) ampIndex = idx;
                        arg = [aSym autoGensym];
                        [SymbolTable setSymbol:arg];
                    }
                    [lock lock];
                    [symArgs setObject:arg atIndexedSubscript:idx];
                    [lock unlock];
                }];
                if (ampIndex > -1) [symArgs removeObjectAtIndex:ampIndex];  // Remove '&' variadic symbol
                [elem setValue:symArgs];
                [ast update:elem atIndex:i];
                continue;
            } else if ([sym position] == 0 && [keywords containsObject:sym]) {
                continue;
            } else {
//                JSSymbol *aSym = [self updateSymbol:sym array:symbols];
//                if (aSym) [ast update:aSym atIndex:i];
                [self updateSymbol:[ast nth:i]];
            }
//            else {
//                [self matchAndUpdate:[ast nth:i] array:symbols];
//            }
        } else if ([JSList isList:elem]) {  // (fn, args)
            elem = [self updateBindingsForAST:elem];
        } else if ([JSVector isVector:elem]) {
            elem = [self updateBindingsForVector:elem];
        } else if ([JSHashMap isHashMap:elem]) {
            elem = [self updateBindingsForHashMap:elem];
        }
    }
    return ast;
}

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
        _initialName = name;
        _arity = -2;
        _initialArity = -2;
        [self updateArity];
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
        _initialName = [symbol initialValue];
        _meta = [symbol meta];
        _arity = arity;
        _initialArity = arity;
        _position = position;
        _hasNArity = [symbol hasNArity];
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithArity:(NSInteger)arity string:(NSString *)string {
    return [self initWithArity:arity position:-1 string:string];
}

- (instancetype)initWithArity:(NSInteger)arity position:(NSInteger)position string:(NSString *)string {
    if (arity < -1) [[[JSError alloc] initWithDescription:FunctionArityError] throw];
    self = [super init];
    if (self) {
        _name = string;
        _initialName = _name;
        _arity = arity;
        _initialArity = arity;
        _position = position;
        [self updateArity];
    }
    return self;
}

- (instancetype)initWithMeta:(id<JSDataProtocol>)meta symbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = [symbol name];
        _initialName = [symbol initialValue];
        _isFunction = [symbol isFunction];
        _hasNArity = [symbol hasNArity];
        _meta = meta;
    }
    return self;
}

- (instancetype)initWithMeta:(_Nullable id<JSDataProtocol>)meta name:(NSString *)name {
    self = [super init];
    if (self) {
        [self bootstrap];
        _name = name;
        _initialName = name;
        _meta = meta;
        [self updateArity];
    }
    return self;
}

- (void)bootstrap {
    _position = -1;
}

- (void)updateArity {
    _hasNArity = _arity == -1;
    _isFunction = _arity >= -1;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"symbol";
}

- (BOOL)isGensym {
    return [_name isNotEqualTo:_initialName];
}

- (JSSymbol *)gensym {
    NSUInteger count = [_name count];
    //NSUInteger count = [_name count] - 1;
    //if ([[_name substringFromIndex:count] isEqual:@"#"] && _position == 0) {
    if (_position == 0) {
        _name = [[NSString alloc] initWithFormat:@"%@__%ld__auto__", [_name substringToIndex:count], [SymbolTable counter]];
    }
    return self;
}

- (JSSymbol *)autoGensym {
    NSUInteger count = [_name count];
    //NSUInteger count = [_name count] - 1;
    //if ([[_name substringFromIndex:count] isEqual:@"#"]) {
        _name = [[NSString alloc] initWithFormat:@"%@__%ld__auto__", [_name substringToIndex:count], [SymbolTable counter]];
    //}
    return self;
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

- (NSString *)value {
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

- (NSString *)string {
    if (_initialArity <= -2) return _name;
    return [[NSString alloc] initWithFormat:@"%@/%@", _name, (_initialArity == -1) ? @"n" : [[NSString alloc] initWithFormat:@"%ld", _initialArity]];
}

- (BOOL)isEqualToName:(NSString *)name {
    return [_name isEqual:name];
}

- (BOOL)isEqual:(id)symbol {
    if (![JSSymbol isSymbol:symbol]) return NO;
    return [_name isEqual:[symbol name]] && _arity == [symbol arity];
}

- (NSUInteger)hash {
    return [_name hash] + _arity + 2;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[JSSymbol alloc] initWithName:_name];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[JSSymbol alloc] initWithName:_name];
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - %@ meta: %@>", NSStringFromClass([self class]), self, _name, _meta];
}

@end
