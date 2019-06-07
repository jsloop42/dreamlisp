//
//  Core.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Core.h"

/** Core functions exposed to the environment. */
@implementation Core {
    Env *_env;
    Reader *_reader;
    Printer *_printer;
    Terminal *_terminal;
}

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)bootstrap {
    _env = [[Env alloc] init];
    [_env setModuleName:[Const coreModuleName]];
    [_env setIsUserDefined:NO];
    _reader = [Reader new];
    _printer = [Printer new];
    _terminal = [Terminal new];
    [self addArithmeticFunctions];
    [self addComparisonFunctions];
    [self addPrintFunctions];
    [self addListFunctions];
    [self addEvalFunctions];
    [self addAtomFunctions];
    [self addInvokeFunctions];
    [self addPredicateFunctions];
    [self addSymbolFunctions];
    [self addKeywordFunctions];
    [self addVectorFunctions];
    [self addHashMapFunctions];
    [self addIOFunctions];
    [self addMetaFunctions];
    [self addMiscFunctions];
}

/** Perform modulus with double numbers. */
double dmod(double a, double n) {
    return a - n * floor(a / n);
}

/** Arithmetic functions takes variadic arguments. If no arguments are given, default identities are returned if present, @c 0 for @c (+) and @c 1 @c (*). */
- (void)addArithmeticFunctions {
    JSFunction *fn = nil;

    id<JSDataProtocol>(^calc)(NSMutableArray *args, SEL sel) = ^id<JSDataProtocol>(NSMutableArray *args, SEL sel) {
        NSDecimalNumber *num = [NSDecimalNumber new];
        NSUInteger len = [args count];
        NSUInteger i = 0;
        BOOL isDouble = NO;
        JSNumber *aNum = nil;
        if (len == 0) {
            [TypeUtils checkArity:args arity:1];
        } else if (len == 1) {
            return [JSNumber dataToNumber:args[0]];
        }
        if (len >= 2) {
            aNum = [JSNumber dataToNumber:args[0]];
            if ([aNum isDouble]) isDouble = YES;
            NSDecimalNumber *n = [aNum value];
            if ([n respondsToSelector:sel]) {
                aNum = [JSNumber dataToNumber:args[1]];
                if ([aNum isDouble]) isDouble = YES;
                num = objc_msgSend(n, sel, [aNum value]);
            }
        }
        if (len > 2) {
            for (i = 2; i < len; i++) {
                aNum = [JSNumber dataToNumber:args[i]];
                if ([aNum isDouble]) isDouble = YES;
                num = objc_msgSend(num, sel, [aNum value]);
            }
        }
        if (isDouble) return [[JSNumber alloc] initWithDoubleNumber:num];
        return [[JSNumber alloc] initWithNumber:num];
    };

    JSFunction *add = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args isEmpty]) return [[JSNumber alloc] initWithInt:0];
        return calc(args, @selector(decimalNumberByAdding:));
    } name:@"+/n"];
    [add setArgsCount:-1];
    [_env setObject:add forKey:[[JSSymbol alloc] initWithFunction:add name:@"+" moduleName:[Const coreModuleName]]];

    JSFunction *subtract = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args count] == 1) {
            JSNumber *n = [JSNumber dataToNumber:args[0]];
            JSNumber *ret = nil;
            if ([n isDouble]) {
                ret = [[JSNumber alloc] initWithDoubleNumber:[[NSDecimalNumber alloc] initWithDouble:(-1.0 * [n doubleValue])]];
            } else {
                ret = [[JSNumber alloc] initWithNumber:[[NSDecimalNumber alloc] initWithInteger:(-1 * [n integerValue])]];
            }
            return ret;
        }
        return calc(args, @selector(decimalNumberBySubtracting:));
    } name:@"-/n"];
    [subtract setArgsCount:-1];
    [_env setObject:subtract forKey:[[JSSymbol alloc] initWithFunction:subtract name:@"-" moduleName:[Const coreModuleName]]];

    JSFunction *multiply = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args isEmpty]) return [[JSNumber alloc] initWithInt:1];
        return calc(args, @selector(decimalNumberByMultiplyingBy:));
    } name:@"*/n"];
    [multiply setArgsCount:-1];
    [_env setObject:multiply forKey:[[JSSymbol alloc] initWithFunction:multiply name:@"*" moduleName:[Const coreModuleName]]];

    JSFunction *divide = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args count] == 1) [args insertObject:[[JSNumber alloc] initWithInteger:1] atIndex:0];
        return calc(args, @selector(decimalNumberByDividingBy:));
    } name:@"//n"];
    [divide setArgsCount:-1];
    [_env setObject:divide forKey:[[JSSymbol alloc] initWithFunction:divide name:@"/" moduleName:[Const coreModuleName]]];

    id<JSDataProtocol>(^mod)(NSMutableArray *args) = ^id<JSDataProtocol>(NSMutableArray *args) {
        [TypeUtils checkArity:args arity:2];
        JSNumber *lhs = [JSNumber dataToNumber:[args first] fnName:@"mod/2"];
        JSNumber *rhs = [JSNumber dataToNumber:[args second] fnName:@"mod/2"];
        JSNumber *ret = nil;
        if ([lhs isDouble] || [rhs isDouble]) {
            ret = [[JSNumber alloc] initWithDouble:dmod([lhs doubleValue], [rhs doubleValue])];
        } else {
            ret = [[JSNumber alloc] initWithInteger:[lhs integerValue] % [rhs integerValue]];
        }
        return ret;
    };
    fn = [[JSFunction alloc] initWithFn:mod argCount:2 name:@"mod/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"mod" moduleName:[Const coreModuleName]]];
}

/** Checks if the given data are equal.  */
- (BOOL)isEqual:(id<JSDataProtocol>)lhs rhs:(id<JSDataProtocol>)rhs {
    if ([JSNumber isNumber:lhs] && [JSNumber isNumber:rhs]) {
        return [(JSNumber *)lhs isEqual:(JSNumber *)rhs];
    } else if ([JSSymbol isSymbol:lhs] && [JSSymbol isSymbol:rhs]) {
        return [(JSSymbol *)lhs isEqual:(JSSymbol *)rhs];
    } else if ([JSString isString:lhs] && [JSString isString:rhs]) {
        return [(JSString *)lhs isEqual:(JSString *)rhs];
    } else if ([JSKeyword isKeyword:lhs] && [JSKeyword isKeyword:rhs]) {
        return [(JSKeyword *)lhs isEqual:(JSKeyword *)rhs];
    } else if (([JSList isList:lhs] && [JSList isList:rhs]) ||
               ([JSList isList:lhs] && [JSVector isVector:rhs]) ||
               ([JSVector isVector:lhs] && [JSList isList:rhs]) ||
               ([JSVector isVector:lhs] && [JSVector isVector:rhs])) {
        return [(JSList *)lhs isEqual:(JSList *)rhs];
    } else if ([JSHashMap isHashMap:lhs] && [JSHashMap isHashMap:rhs]) {
        return [(JSHashMap *)lhs isEqual:(JSHashMap *)rhs];
    } else if ([JSNil isNil:lhs] && [JSNil isNil:rhs]) {
        return YES;
    } else if ([JSBool isBool:lhs] && [JSBool isBool:rhs]) {
        return [(JSBool *)lhs isEqual:(JSBool *)rhs];
    }
    return NO;
}

/** Functions that does monotonic comparison. If only one argument is given, @c true is returned. */
- (void)addComparisonFunctions {
    id<JSDataProtocol>(^compare)(NSMutableArray *args, SEL sel) = ^id<JSDataProtocol>(NSMutableArray *args, SEL sel) {
        BOOL ret = YES;
        NSUInteger i = 0;
        NSUInteger len = [args count];
        if (len >= 2) {
            for (i = 0; i < len - 1; i++) {
                ret = (BOOL)objc_msgSend([(JSNumber *)args[i] value], sel, [(JSNumber *)args[i + 1] value]);
                if (!ret) { break; }
            }
        } else if (len == 0) {
            [TypeUtils checkArity:args arity:1];
        }
        return [[JSBool alloc] initWithBool:ret];
    };

    JSFunction *lessThan = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isLessThan:));
    } name:@"</n"];
    [lessThan setArgsCount:-1];
    [_env setObject:lessThan forKey:[[JSSymbol alloc] initWithFunction:lessThan name:@"<" moduleName:[Const coreModuleName]]];

    JSFunction *greaterThan = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isGreaterThan:));
    } name:@">/n"];
    [greaterThan setArgsCount:-1];
    [_env setObject:greaterThan forKey:[[JSSymbol alloc] initWithFunction:greaterThan name:@">" moduleName:[Const coreModuleName]]];

    JSFunction *lessThanOrEqualTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isLessThanOrEqualTo:));
    } name:@"<=/n"];
    [lessThanOrEqualTo setArgsCount:-1];
    [_env setObject:lessThanOrEqualTo forKey:[[JSSymbol alloc] initWithFunction:lessThanOrEqualTo name:@"<=" moduleName:[Const coreModuleName]]];

    JSFunction *greaterThanOrEqualTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isGreaterThanOrEqualTo:));
    } name:@">=/n"];
    [greaterThanOrEqualTo setArgsCount:-1];
    [_env setObject:greaterThanOrEqualTo forKey:[[JSSymbol alloc] initWithFunction:greaterThanOrEqualTo name:@">=" moduleName:[Const coreModuleName]]];

    JSFunction *equalTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        BOOL ret = YES;
        NSUInteger len = [args count];
        NSUInteger i = 0;
        if (len >= 2) {
            for (i = 0; i < len - 1; i++) {
                ret = [self isEqual:(id<JSDataProtocol>)args[i] rhs:(id<JSDataProtocol>)args[i + 1]];
                if (!ret) { break; }
            }
        } else if (len == 0) {
            [TypeUtils checkArity:args arity:1];
        }
        return [[JSBool alloc] initWithBool:ret];
    } name:@"=/n"];
    [equalTo setArgsCount:-1];
    [_env setObject:equalTo forKey:[[JSSymbol alloc] initWithFunction:equalTo name:@"=" moduleName:[Const coreModuleName]]];
}

/** Add various string functions that returns a string or prints it to stdout. */
- (void)addPrintFunctions {
    Core * __weak weakSelf = self;
    JSFunction *fn = nil;
    id<JSDataProtocol>(^println)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];  // readably false means for viewing in REPL (println "a") -> a nil
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:println argCount:-1 name:@"println/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"println" moduleName:[Const coreModuleName]]];

    id<JSDataProtocol>(^prn)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:true]];  // prints strings as is with quotes (prn "a") -> "a" nil
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:prn argCount:-1 name:@"prn/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"prn" moduleName:[Const coreModuleName]]];

    id<JSDataProtocol>(^print)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return nil;
    };
    // prints to stdout without returning nil
    fn = [[JSFunction alloc] initWithFn:print argCount:-1 name:@"print/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"print" moduleName:[Const coreModuleName]]];
    id<JSDataProtocol>(^prstr)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:true]];
        }
        return [[JSString alloc] initWithString:[ret componentsJoinedByString:@" "]];
    };
    // returns escaped strings (pr-str "a") -> "\"a\""
    fn = [[JSFunction alloc] initWithFn:prstr argCount:-1 name:@"pr-str/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"pr-str" moduleName:[Const coreModuleName]]];

    id<JSDataProtocol>(^str)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];
        }
        return [[JSString alloc] initWithString:[ret componentsJoinedByString:@""]];
    };
    // returns strings as such (str "a") -> "a"
    fn = [[JSFunction alloc] initWithFn:str argCount:-1 name:@"str/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"str" moduleName:[Const coreModuleName]]];
}

- (void)addListFunctions {
    JSFunction *fn = nil;
    /** Create a list from the given elements. */
    id<JSDataProtocol>(^list)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSList alloc] initWithArray:xs];
    };
    fn = [[JSFunction alloc] initWithFn:list argCount:-1 name:@"list/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"list" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a list */
    id<JSDataProtocol>(^listp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[[(id<JSDataProtocol>)[xs first] dataType] isEqual:@"JSList"]];
    };
    fn = [[JSFunction alloc] initWithFn:listp argCount:1 name:@"list?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"list?" moduleName:[Const coreModuleName]]];

    /** Checks if the given list contains no elements. */
    id<JSDataProtocol>(^emptyp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        BOOL ret = YES;
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        ret = [JSString isString:first] ? [(JSString *)first isEmpty] : [[JSList dataToList:first fnName:@"empty?/1"] isEmpty];
        return [[JSBool alloc] initWithBool:ret];
    };
    fn = [[JSFunction alloc] initWithFn:emptyp argCount:1 name:@"empty?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"empty?" moduleName:[Const coreModuleName]]];

    /** Returns the number of elements in the given list. */
    id<JSDataProtocol>(^count)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSNil isNil:first]) return [[JSNumber alloc] initWithInteger:0];
        return [[JSNumber alloc] initWithInteger:[JSString isString:first]
                ? [(JSString *)first count]
                : [[JSList dataToList:first fnName:@"count/1"] count]];
    };
    fn = [[JSFunction alloc] initWithFn:count argCount:1 name:@"count/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"count" moduleName:[Const coreModuleName]]];

    /**
     Takes any element and adds it to the first of the given list.

     (cons 1 '(2 3 4)) ; (1 2 3 4)
     (cons '(1) '(2 3 4)) ; ((1) 2 3 4)
     */
    id<JSDataProtocol>(^cons)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs second];
        NSMutableArray *arr = [[[JSList dataToList:data position:2 fnName:@"cons/2"] value] mutableCopy];
        [arr insertObject:(id<JSDataProtocol>)[xs first] atIndex:0];
        return [[JSList alloc] initWithArray:arr];
    };
    fn = [[JSFunction alloc] initWithFn:cons argCount:2 name:@"cons/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"cons" moduleName:[Const coreModuleName]]];

    /**
     Takes two lists or vectors, combines them and returns a list

     (concat '(-2 -1 0) '(1 2 3 4)) ; (-2 -1 0 1 2 3 4)
     (concat ["a"] ["b"]) ; ("a" "b")
     */
    id<JSDataProtocol>(^concat)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        NSMutableArray *arr = [NSMutableArray new];
        NSUInteger i = 0;
        NSUInteger j = 0;
        NSUInteger len = [xs count];
        NSUInteger jlen = 0;
        JSList *list = nil;
        id<JSDataProtocol> data = nil;
        for (i = 0; i < len; i++) {
            data = [xs nth:i];
            list = [JSVector dataToList:data fnName:@"concat/n"];
            jlen = [list count];
            for (j = 0; j < jlen; j++) {
                [arr addObject:[list nth:j]];
            }
        }
        return [[JSList alloc] initWithArray:arr];
    };
    fn = [[JSFunction alloc] initWithFn:concat argCount:-1 name:@"concat/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"concat" moduleName:[Const coreModuleName]]];

    /**
     Returns the nth indexed element from the list.

     (nth [1 2 3] 2) ; 3
     (nth [1 2 3] 0) ; 1
     */
    id<JSDataProtocol>(^nth)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        NSMutableArray *list = [[JSVector dataToList:first position:1 fnName:@"nth/2"] value];
        JSNumber *num = [JSNumber dataToNumber:second position:2 fnName:@"nth/2"];
        NSUInteger n = [num integerValue];
        [TypeUtils checkIndexBounds:list index:n];
        return [list nth:n];
    };
    fn = [[JSFunction alloc] initWithFn:nth argCount:2 name:@"nth/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"nth" moduleName:[Const coreModuleName]]];

    /** Returns the first element of the list. If the list is empty, this returns nil. */
    id<JSDataProtocol>(^first)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> list = [xs first];
        if ([xs isEmpty] || [JSNil isNil:list]) return [JSNil new];
        id<JSDataProtocol> first = nil;
        if ([JSList isKindOfList:list]) {
            first = (id<JSDataProtocol>)[(JSList *)list first];
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"first/1", @"'list' or 'vector'", 1, [list dataTypeName]] throw];
        }
        return (first == nil) ? [JSNil new] : first;
    };
    fn = [[JSFunction alloc] initWithFn:first argCount:1 name:@"first/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"first" moduleName:[Const coreModuleName]]];

    /** Returns a list without the first element. If the list is empty, then the list is returned. */
    id<JSDataProtocol>(^rest)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> data = [xs first];
        if ([JSNil isNil:data]) return [JSList new];
        JSList *list = [JSVector dataToList:data position:1 fnName:@"rest/1"];
        return [list isEmpty] ? [JSList new] : (JSList *)[list rest];
    };
    fn = [[JSFunction alloc] initWithFn:rest argCount:1 name:@"rest/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"rest" moduleName:[Const coreModuleName]]];

    /**
     Takes a function and a list and applies the function to each element of the list and return the resulting list.

     (map (fn* (x) (* x x)) '(2 3 4 5)) ; (4 9 16 25)
     (map count/1 '([1] [2 3] [4 5 6] [7 8] [9])) ; (1 2 3 2 1)
     */
    id<JSDataProtocol>(^map)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        JSFunction *fn = [JSFunction dataToFunction:first];
        NSMutableArray *list = [[JSList dataToList:[xs second] fnName:@"map/2"] value];
        NSUInteger i = 0;
        NSUInteger len = [list count];
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[fn apply:[@[[list nth:i]] mutableCopy]]];
        }
        return [[JSList alloc] initWithArray:ret];
    };
    fn = [[JSFunction alloc] initWithFn:map argCount:2 name:@"map/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"map" moduleName:[Const coreModuleName]]];

    /**
     Takes a vector and n elements, appends the elements to the vector and return resulting new vector. The original vector remains unchanged.
     If a list is given the elements are appended to the head of the list giving a reversed list.

     (conj [1] 2 3 4) ; [1 2 3 4]
     (conj '(1) 2 3 4) ; (4 3 2 1)
     */
    id<JSDataProtocol>(^conj)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        NSMutableArray *list = nil;
        NSMutableArray *rest = [xs rest];
        if ([JSVector isVector:first]) {
            list = [(JSVector *)first value];
            [list addObjectsFromArray:rest];
            return [[JSVector alloc] initWithArray:list];
        } else if ([JSList isList:first]) {
            list = [(JSList *)first value];
        }
        return [[JSList alloc] initWithArray:[[rest reverse] arrayByAddingObjectsFromArray:list]];
    };
    fn = [[JSFunction alloc] initWithFn:conj argCount:-1 name:@"conj/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"conj" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is iteratable, which is a list or a vector. */
    id<JSDataProtocol>(^sequentialp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSList isList:first] || [JSVector isVector:first])];
    };
    fn = [[JSFunction alloc] initWithFn:sequentialp argCount:1 name:@"sequential?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"sequential?" moduleName:[Const coreModuleName]]];

    /**
     Takes a list, vector or a string and returns a list containing individual elements that can be iterated.

     (seq '(1 2 3)) ; (1 2 3)
     (seq [1 2 3]) ; (1 2 3)
     (seq "abc") ; ("a" "b" "c")
     */
    id<JSDataProtocol>(^seq)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        if ([xs count] == 0) return [JSNil new];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSList isList:first]) {
            JSList *list = (JSList *)first;
            if ([list count] == 0) return [JSNil new];
            return list;
        }
        if ([JSVector isVector:first]) {
            JSVector *vec = (JSVector *)first;
            if ([vec count] == 0) return [JSNil new];
            return [vec list];
        }
        if ([JSString isString:first]) {
            NSMutableArray *arr = [NSMutableArray new];
            NSUInteger i = 0;
            JSString *str = (JSString *)first;
            NSString *string = [str value];
            NSUInteger len = [string count];
            if (len == 0) return [JSNil new];
            for(i = 0; i < len; i++) {
                [arr addObject:[[NSString alloc] initWithFormat:@"%c", [string characterAtIndex:i]]];
            }
            return [[JSList alloc] initWithArray:arr];
        }
        if ([JSNil isNil:first]) return (JSNil *)first;
        [[[JSError alloc] initWithDescription:SequenceError] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:seq argCount:1 name:@"seq/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"seq" moduleName:[Const coreModuleName]]];
}

/** Exposes the reader which is used for eval */
- (void)addEvalFunctions {
    Core * __weak weakSelf = self;
    JSFunction *fn = nil;

    /** Takes an expression in string form, tokenizes it returning the expression. */
    id<JSDataProtocol>(^readString)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        Core *this = weakSelf;
        NSMutableArray *exprs = [this->_reader readString:[[JSString dataToString:[xs first] fnName:@"read-string/1"] value]];
        NSUInteger len = [exprs count];
        return (len == 0) ? [JSNil new] : exprs[len - 1];
    };
    fn = [[JSFunction alloc] initWithFn:readString argCount:1 name:@"read-string/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"read-string" moduleName:[Const coreModuleName]]];

    /** Read content of a file as string. Should be used for smaller files only. */
    id<JSDataProtocol>(^slurp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSString alloc] initWithContentsOfFile:[[JSString dataToString:[xs first] fnName:@"slurp/1"] value]];
    };
    fn = [[JSFunction alloc] initWithFn:slurp argCount:1 name:@"slurp/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"slurp" moduleName:[Const coreModuleName]]];
}

- (void)addAtomFunctions {
    JSFunction *fn = nil;

    /** Create an atom with the given element as its value. */
    id<JSDataProtocol>(^atom)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSAtom alloc] initWithData:[xs first]];
    };
    fn = [[JSFunction alloc] initWithFn:atom argCount:1 name:@"atom/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"atom" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is an atom. */
    id<JSDataProtocol>(^atomp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSAtom isAtom:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:atomp argCount:1 name:@"atom?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"atom?" moduleName:[Const coreModuleName]]];

    /** Dereferences an atom returning the value it holds. */
    id<JSDataProtocol>(^deref)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return ![JSAtom isAtom:first] ? [JSNil new] : [(JSAtom *)first value];
    };
    fn = [[JSFunction alloc] initWithFn:deref argCount:1 name:@"deref/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"deref" moduleName:[Const coreModuleName]]];

    /** Mutates the value of an atom to the new given value. */
    id<JSDataProtocol>(^reset)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        JSAtom *atom = [JSAtom dataToAtom:first fnName:@"reset!/2"];
        id<JSDataProtocol>value = (id<JSDataProtocol>)[xs second];
        [atom setValue:value];
        return value;
    };
    fn = [[JSFunction alloc] initWithFn:reset argCount:2 name:@"reset!/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"reset!" moduleName:[Const coreModuleName]]];

    /** Takes an atom, a function and arguments, applies the function to the arguments and sets the resulting value as the value of the atom. */
    id<JSDataProtocol>(^swap)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        id<JSDataProtocol> second = (id<JSDataProtocol>)[xs second];
        JSAtom *atom = [JSAtom dataToAtom:first position:1 fnName:@"swap!/n"];
        JSFunction *fn = [JSFunction dataToFunction:second position:2];
        NSMutableArray *more = [xs drop:2];
        [more insertObject:[atom value] atIndex:0];
        [atom setValue:[fn fn](more)];
        return [atom value];
    };
    fn = [[JSFunction alloc] initWithFn:swap argCount:-1 name:@"swap!/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"swap!" moduleName:[Const coreModuleName]]];
}

- (void)addInvokeFunctions {
    JSFunction* fn = nil;

    /** Throws an exception with the given data as its value. */
    id<JSDataProtocol>(^throw)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        @throw [[NSException alloc] initWithName:JSLException reason:JSLException userInfo:@{@"jsdata": (id<JSDataProtocol>)[xs first]}];
    };
    fn = [[JSFunction alloc] initWithFn:throw argCount:1 name:@"throw/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"throw" moduleName:[Const coreModuleName]]];

    /** Takes a function and a list of arguments, invokes the function with the elements in the list as its arguments. */
    id<JSDataProtocol>(^apply)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSFunction *fn = [JSFunction dataToFunction:[xs first] position:1];
        NSMutableArray *last = [[[JSList dataToList:[xs last] position:[xs count] fnName:@"apply/n"] value] mutableCopy];
        NSMutableArray *params = [NSMutableArray new];
        NSMutableArray *args = [xs mutableCopy];
        if ([args count] >= 2) {
            args = [args drop:1];
            args = [args dropLast];
        }
        if (args) params = args;
        [params addObjectsFromArray:last];
        return [fn apply:params];
    };
    fn = [[JSFunction alloc] initWithFn:apply argCount:-1 name:@"apply/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"apply" moduleName:[Const coreModuleName]]];
}

- (void)addPredicateFunctions {
    JSFunction *fn = nil;

    /** Checks if the given element is @c nil. */
    id<JSDataProtocol>(^nilp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSNil isNil:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:nilp argCount:1 name:@"nil?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"nil?" moduleName:[Const coreModuleName]]];

    /** Checks if the given value is @c true. */
    id<JSDataProtocol>(^truep)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSBool isBool:data] && [(JSBool *)data value] == YES)];
    };
    fn = [[JSFunction alloc] initWithFn:truep argCount:1 name:@"true?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"true?" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is @c false. */
    id<JSDataProtocol>(^falsep)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSBool isBool:data] && [(JSBool *)data value] == NO)];
    };
    fn = [[JSFunction alloc] initWithFn:falsep argCount:1 name:@"false?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"false?" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a @c string. */
    id<JSDataProtocol>(^stringp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSString isString:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:stringp argCount:1 name:@"string?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"string?" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a @c number. */
    id<JSDataProtocol>(^numberp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSNumber isNumber:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:numberp argCount:1 name:@"number?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"number?" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a @c function. */
    id<JSDataProtocol>(^fnp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSFunction isFunction:first]) {
            JSFunction *fn = (JSFunction *)first;
            if (![fn isMacro]) return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    fn = [[JSFunction alloc] initWithFn:fnp argCount:1 name:@"fn?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"fn?" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a @c macro. */
    id<JSDataProtocol>(^macrop)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSFunction isFunction:first] && [(JSFunction *)first isMacro])];
    };
    fn = [[JSFunction alloc] initWithFn:macrop argCount:1 name:@"macro?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"macro?" moduleName:[Const coreModuleName]]];
}

- (void)addSymbolFunctions {
    JSFunction *fn = nil;

    /** Creates a symbol from the given string. */
    id<JSDataProtocol>(^symbol)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSSymbol alloc] initWithName:[(JSString *)[xs first] value]];
    };
    fn = [[JSFunction alloc] initWithFn:symbol argCount:1 name:@"symbol/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"symbol" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a symbol. */
    id<JSDataProtocol>(^symbolp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSSymbol isSymbol:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:symbolp argCount:1 name:@"symbol?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"symbol?" moduleName:[Const coreModuleName]]];
}

- (void)addKeywordFunctions {
    JSFunction *fn = nil;

    /** Create a keyword from the given element. */
    id<JSDataProtocol>(^keyword)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol> )[xs first];
        if ([JSKeyword isKeyword:first]) return first;
        return ([JSString isString:first]) ? [[JSKeyword alloc] initWithString:[(JSString *)first value]] :[JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:keyword argCount:1 name:@"keyword/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"keyword" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a keyword. */
    id<JSDataProtocol>(^keywordp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSKeyword isKeyword:first] || ([NSString isString:first] && [JSKeyword isEncodedKeyword:(NSString *)first]))];
    };
    fn = [[JSFunction alloc] initWithFn:keywordp argCount:1 name:@"keyword?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"keyword?" moduleName:[Const coreModuleName]]];
}

- (void)addVectorFunctions {
    JSFunction *fn = nil;

    /** Create a vector with the given elements. */
    id<JSDataProtocol>(^vector)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSVector alloc] initWithArray:xs];
    };
    fn = [[JSFunction alloc] initWithFn:vector argCount:-1 name:@"vector/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"vector" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a vector. */
    id<JSDataProtocol>(^vectorp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSVector isVector:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:vectorp argCount:1 name:@"vector?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"vector?" moduleName:[Const coreModuleName]]];
}

- (void)addHashMapFunctions {
    JSFunction *fn = nil;

    /** Create a hash map with given key value pair. The first element is taken as a key and the next element as its value and so on.*/
    id<JSDataProtocol>(^hashmap)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSHashMap alloc] initWithArray:xs];
    };
    fn = [[JSFunction alloc] initWithFn:hashmap argCount:-1 name:@"hash-map/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"hash-map" moduleName:[Const coreModuleName]]];

    /** Checks if the given element is a hash map. */
    id<JSDataProtocol>(^mapp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSHashMap isHashMap:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:mapp argCount:1 name:@"map?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"map?" moduleName:[Const coreModuleName]]];

    /**
     Takes a hash map and key value pairs, add them to the hash map and return a resulting new hash map.

     (assoc {:a 1} :b 2 :c 3) ; {:a 1 :c 4 :b 3}
     */
    id<JSDataProtocol>(^assoc)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:3 predicate:ArityPredicateMin];
        [TypeUtils checkArity:xs arity:3 predicate:ArityPredicateOdd];
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"assoc/n"];
        NSMapTable *table = [[first value] mutableCopy];
        NSMapTable *rest = [[[JSHashMap alloc] initWithArray:[xs rest]] value];
        return [[JSHashMap alloc] initWithMapTable:[table assoc:rest]];
    };
    fn = [[JSFunction alloc] initWithFn:assoc argCount:-1 name:@"assoc/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"assoc" moduleName:[Const coreModuleName]]];

    /**
     Takes a hash map and keys, removes the value associated for the key if present and returns the resulting new hash map.

     (dissoc {:a 1 :b 2} :b :c) ; {:a 1}
     */
    id<JSDataProtocol>(^dissoc)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"dissoc/n"];
        NSArray *keys = [xs rest];
        NSMapTable *table = [[first value] mutableCopy];
        return [[JSHashMap alloc] initWithMapTable:[table dissoc:keys]];
    };
    fn = [[JSFunction alloc] initWithFn:dissoc argCount:-1 name:@"dissoc/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"dissoc" moduleName:[Const coreModuleName]]];

    /**
     Takes a hash map and a key and returns the value associated with the key if present. Returns nil otherwise.

     (get {:a 1 :b 2 :c 3} :b) ; 2
     (get {:a 1 :b 2 :c 3} :v) ; nil
     */
    id<JSDataProtocol>(^get)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        if ([JSHashMap isHashMap:data]) {
            JSHashMap *first = (JSHashMap *)data;
            id<JSDataProtocol> ret = (id<JSDataProtocol>)[first objectForKey:[xs second]];
            if (ret) return ret;
        }
        return [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:get argCount:2 name:@"get/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"get" moduleName:[Const coreModuleName]]];

    /**
     Checks if the given hash map contains the key.

     (contains? {:a 1 :b 2 :c 3} :a) ; true
     (contains? {:a 1 :b 2 :c 3} 3) ; false
     (contains? {:a 1 :b 2 :c 3} :v) ;false
     */
    id<JSDataProtocol>(^contains)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"contains?/2"];
        return [[JSBool alloc] initWithBool:[first containsKey:[xs second]]];
    };
    fn = [[JSFunction alloc] initWithFn:contains argCount:2 name:@"contains?/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"contains?" moduleName:[Const coreModuleName]]];

    /** Returns a list containing the hash map keys. */
    id<JSDataProtocol>(^keys)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"keys/1"];
        return [[JSList alloc] initWithArray:[first allKeys]];
    };
    fn = [[JSFunction alloc] initWithFn:keys argCount:1 name:@"keys/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"keys" moduleName:[Const coreModuleName]]];

    /** Returns a list containing the hash map values. */
    id<JSDataProtocol>(^vals)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"vals/1"];
        return [[JSList alloc] initWithArray:[first allObjects]];
    };
    fn = [[JSFunction alloc] initWithFn:vals argCount:1 name:@"vals/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"vals" moduleName:[Const coreModuleName]]];
}

- (void)addIOFunctions {
    Core * __weak weakSelf = self;
    JSFunction *fn = nil;

    /** Reads a line from the stdin with the given prompt displayed. */
    id<JSDataProtocol>(^readline)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        Core *this = weakSelf;
        return [[JSString alloc] initWithString:[this->_terminal
                                                 readlineWithPrompt:[[[JSString dataToString:[xs first] fnName:@"readline/1"] value] UTF8String]]];
    };
    fn = [[JSFunction alloc] initWithFn:readline argCount:1 name:@"readline/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"readline" moduleName:[Const coreModuleName]]];
}

- (void)addMetaFunctions {
    JSFunction *fn = nil;

    /**
     Associates the given element with a metadata.

     (def f (with-meta :foo 1)) ; :foo  The keyword :foo has meta as 1
     */
    id<JSDataProtocol>(^withMeta)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        id<JSDataProtocol> meta = (id<JSDataProtocol>)[xs second];
        if ([JSFunction isFunction:first]) return [[JSFunction alloc] initWithMeta:meta func:(JSFunction *)first];
        if ([JSString isString:first]) return [[JSString alloc] initWithMeta:meta string:(JSString *)first];
        if ([JSKeyword isKeyword:first]) return [[JSKeyword alloc] initWithMeta:meta keyword:(JSKeyword *)first];
        if ([JSSymbol isSymbol:first]) return [[JSSymbol alloc] initWithMeta:meta symbol:(JSSymbol *)first];
        if ([JSHashMap isHashMap:first]) return [[JSHashMap alloc] initWithMeta:meta hashmap:(JSHashMap *)first];
        if ([JSList isList:first]) return [[JSList alloc] initWithMeta:meta list:(JSList *)first];
        if ([JSVector isVector:first]) return [[JSVector alloc] initWithMeta:meta vector:(JSVector *)first];
        if ([JSNumber isNumber:first]) return [[JSNumber alloc] initWithMeta:meta number:(JSNumber *)first];
        if ([JSAtom isAtom:first]) return [[JSAtom alloc] initWithMeta:meta atom:(JSAtom *)first];
        return first;
    };
    fn = [[JSFunction alloc] initWithFn:withMeta argCount:2 name:@"with-meta/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"with-meta" moduleName:[Const coreModuleName]]];

    /**
     Return a meta associated with the given element if present or nil.

     (meta (with-meta :foo 1)) ; 1
     */
    id<JSDataProtocol>(^meta)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSFunction isFunction:first]) {
            JSFunction *fn = (JSFunction *)first;
            if ([fn meta]) return [fn meta];
        }
        return [first meta] ? [first meta] : [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:meta argCount:1 name:@"meta/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"meta" moduleName:[Const coreModuleName]]];
}

- (void)addMiscFunctions {
    JSFunction *fn = nil;

    /** Exits the current running process. To be used in REPL only. */
    id<JSDataProtocol>(^exitfn)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        exit(0);
    };
    fn = [[JSFunction alloc] initWithFn:exitfn argCount:0 name:@"exit*/0"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"exit*" moduleName:[Const coreModuleName]]];

    /** Returns the current timestamp in milliseconds. */
    id<JSDataProtocol>(^timems)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:0];
        return [[JSNumber alloc] initWithInteger:[Utils timestamp]];
    };
    fn = [[JSFunction alloc] initWithFn:timems argCount:0 name:@"time-ms/0"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"time-ms" moduleName:[Const coreModuleName]]];
}

- (Env *)env {
    return _env;
}

@end
