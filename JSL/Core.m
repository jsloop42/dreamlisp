//
//  Core.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Core.h"

static NSString *_description = @"The core module.";

/** Core functions exposed to the environment. */
@implementation Core {
    Env *_env;
    Reader *_reader;
    Printer *_printer;
    Terminal *_terminal;
    id<JSLDelegate> __weak _delegate;
    NSData *_allModuleSortHint;
}

@synthesize delegate = _delegate;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)bootstrap {
    _env = [[Env alloc] init];
    [_env setModuleName:[Const coreModuleName]];
    [_env setModuleDescription:_description];
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
    [self addModuleFunctions];
}

#pragma mark - Arithmetic

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

    #pragma mark add
    JSFunction *add = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args isEmpty]) return [[JSNumber alloc] initWithInt:0];
        return calc(args, @selector(decimalNumberByAdding:));
    } name:@"+/n"];
    [add setArgsCount:-1];
    [_env setObject:add forKey:[[JSSymbol alloc] initWithFunction:add name:@"+" moduleName:[Const coreModuleName]]];

    #pragma mark subtract
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

    #pragma mark multiply
    JSFunction *multiply = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args isEmpty]) return [[JSNumber alloc] initWithInt:1];
        return calc(args, @selector(decimalNumberByMultiplyingBy:));
    } name:@"*/n"];
    [multiply setArgsCount:-1];
    [_env setObject:multiply forKey:[[JSSymbol alloc] initWithFunction:multiply name:@"*" moduleName:[Const coreModuleName]]];

    #pragma mark divide
    JSFunction *divide = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        if ([args count] == 1) [args insertObject:[[JSNumber alloc] initWithInteger:1] atIndex:0];
        return calc(args, @selector(decimalNumberByDividingBy:));
    } name:@"//n"];
    [divide setArgsCount:-1];
    [_env setObject:divide forKey:[[JSSymbol alloc] initWithFunction:divide name:@"/" moduleName:[Const coreModuleName]]];

    #pragma mark mod
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

#pragma mark - Comparison

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

    #pragma mark <
    JSFunction *lessThan = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isLessThan:));
    } name:@"</n"];
    [lessThan setArgsCount:-1];
    [_env setObject:lessThan forKey:[[JSSymbol alloc] initWithFunction:lessThan name:@"<" moduleName:[Const coreModuleName]]];

    #pragma mark >
    JSFunction *greaterThan = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isGreaterThan:));
    } name:@">/n"];
    [greaterThan setArgsCount:-1];
    [_env setObject:greaterThan forKey:[[JSSymbol alloc] initWithFunction:greaterThan name:@">" moduleName:[Const coreModuleName]]];

    #pragma mark <=
    JSFunction *lessThanOrEqualTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isLessThanOrEqualTo:));
    } name:@"<=/n"];
    [lessThanOrEqualTo setArgsCount:-1];
    [_env setObject:lessThanOrEqualTo forKey:[[JSSymbol alloc] initWithFunction:lessThanOrEqualTo name:@"<=" moduleName:[Const coreModuleName]]];

    #pragma mark >=
    JSFunction *greaterThanOrEqualTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isGreaterThanOrEqualTo:));
    } name:@">=/n"];
    [greaterThanOrEqualTo setArgsCount:-1];
    [_env setObject:greaterThanOrEqualTo forKey:[[JSSymbol alloc] initWithFunction:greaterThanOrEqualTo name:@">=" moduleName:[Const coreModuleName]]];

    #pragma mark =
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

#pragma mark - Print

/** Add various string functions that returns a string or prints it to stdout. */
- (void)addPrintFunctions {
    Core * __weak weakSelf = self;
    JSFunction *fn = nil;

    #pragma mark println
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

    #pragma mark prn
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

    #pragma mark print
    id<JSDataProtocol>(^print)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];
        }
        info3(@"", @"%@", [ret componentsJoinedByString:@" "]);
        return nil;
    };
    // prints to stdout without newline and  without returning nil
    fn = [[JSFunction alloc] initWithFn:print argCount:-1 name:@"print/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"print" moduleName:[Const coreModuleName]]];

    #pragma mark pr-str
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

    #pragma mark str
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

#pragma mark - List

- (void)addListFunctions {
    JSFunction *fn = nil;

    #pragma mark list
    /** Create a list from the given elements. */
    id<JSDataProtocol>(^list)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSList alloc] initWithArray:xs];
    };
    fn = [[JSFunction alloc] initWithFn:list argCount:-1 name:@"list/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"list" moduleName:[Const coreModuleName]]];

    #pragma mark list?
    /** Checks if the given element is a list */
    id<JSDataProtocol>(^listp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[[(id<JSDataProtocol>)[xs first] dataType] isEqual:@"JSList"]];
    };
    fn = [[JSFunction alloc] initWithFn:listp argCount:1 name:@"list?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"list?" moduleName:[Const coreModuleName]]];

    #pragma mark empty?
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

    #pragma mark count
    /** Returns the number of elements in the given list. */
    id<JSDataProtocol>(^count)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSNil isNil:first]) return [[JSNumber alloc] initWithInteger:0];
        NSUInteger count = 0;
        if ([JSString isString:first]) {
            count = [(JSString *)first count];
        } else if ([JSList isKindOfList:first]) {
            count = [(JSList *)first count];
        } else if ([JSHashMap isHashMap:first]) {
            count = [(JSHashMap *)first count];
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"count/1", @"'sequence' or 'collection'", 1,
              [list dataTypeName]] throw];
        }
        return [[JSNumber alloc] initWithInteger:count];
    };
    fn = [[JSFunction alloc] initWithFn:count argCount:1 name:@"count/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"count" moduleName:[Const coreModuleName]]];

    #pragma mark cons
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

    #pragma mark concat
    /**
     Takes a list of sequences and combines them into one sequence.

     (concat '(-2 -1 0) '(1 2 3 4)) ; (-2 -1 0 1 2 3 4)
     (concat ["a"] ["b"]) ; ["a" "b"]
     (concat "a" "b") ; "ab"
     (concat "a" [1 2 "b"] "c") ; "a12bc"
     */
    id<JSDataProtocol>(^concat)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = [xs first];
        BOOL isList = [JSList isList:first];
        id<JSDataProtocol> elem = nil;
        NSUInteger len = [xs count];
        NSUInteger i = 1;
        if ([JSList isKindOfList:first]) {
            JSList *list = [[JSList alloc] initWithArray:[(JSList *)first value]];
            for (i = 1; i < len; i++) {
                elem = [xs nth:i];
                if ([JSList isKindOfList:elem]) {
                    if (!isList) isList = [JSList isList:elem];
                    [list addObjectsFromList:(JSList *)elem];
                } else if ([JSString isString:elem]) {
                    NSMutableArray *arr = [Utils toArray:elem isNative:YES];
                    [list addObjectsFromArray:arr];
                } else {
                    [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"concat/n", @"'sequence'", i + 1, [elem dataTypeName]] throw];
                }
            }
            return isList ? list : [[JSVector alloc] initWithArray:[list value]];
        } else if ([JSString isString:first]) {
            JSString *str = [JSString mutable];
            [str setMutableValue:[(NSString *)[(JSString *)first value] mutableCopy]];
            for (i = 1; i < len; i++) {
                elem = [xs nth:i];
                if ([JSList isKindOfList:elem]) {
                    [Utils appendStringFromArray:[(JSList *)elem value] string:str];
                } else if ([JSString isString:elem]) {
                    [str append:elem];
                } else {
//                    [str appendString:[elem description]];
                    [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"concat/n", @"'sequence'", i + 1, [elem dataTypeName]] throw];
                }
            }
            return str;
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"concat/n", @"'sequence'", 1, [first dataTypeName]] throw];
        }
        return [JSList new];
    };
    fn = [[JSFunction alloc] initWithFn:concat argCount:-1 name:@"concat/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"concat" moduleName:[Const coreModuleName]]];

    #pragma mark nth
    /**
     Returns the nth element from the sequence.

     (nth 2 [1 2 3]) ; 3
     (nth 0 [1 2 3]) ; 1
     (nth 3 "abcd") ; "d"
     */
    id<JSDataProtocol>(^nth)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        JSNumber *num = [JSNumber dataToNumber:first position:1 fnName:@"nth/2"];
        NSMutableArray *seq = [Utils toArray:second isNative:YES];
        NSUInteger n = [num integerValue];
        [TypeUtils checkIndexBounds:seq index:n];
        return [seq nth:n];
    };
    fn = [[JSFunction alloc] initWithFn:nth argCount:2 name:@"nth/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"nth" moduleName:[Const coreModuleName]]];

    #pragma mark nth-tail
    /** Takes a start, end index, a sequence and returs a sub-sequence within the given indices inclusive. */
    id<JSDataProtocol>(^nthTail)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:3 predicate:ArityPredicateLessThanOrEq];
        id<JSDataProtocol> data = [xs nth:2];
        NSUInteger len = [xs count];
        NSString *fnName = @"nth-tail/3";
        NSInteger start = [[JSNumber dataToNumber:[xs first] position:1 fnName:fnName] integerValue];
        NSInteger end = [[JSNumber dataToNumber:[xs second] position:2 fnName:fnName] integerValue];;
        if ([JSString isString:data]) {
            JSString *str = (JSString *)data;
            return [[JSString alloc] initWithString:[str substringFrom:start to:end]];
        }
        NSUInteger count = 0;
        NSMutableArray *list = [[JSVector dataToList:data position:len fnName:fnName] value];
        count = [list count];
        [TypeUtils checkIndexBoundsCount:count startIndex:start endIndex:end];
        if ([JSList isList:data]) {
            return [[JSList alloc] initWithArray:[(JSList *)data subArrayWithStartIndex:start endIndex:end]];
        } else if ([JSVector isVector:data]) {
            return [[JSVector alloc] initWithArray:[(JSList *)data subArrayWithStartIndex:start endIndex:end]];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithArity, fnName, @"'sequence'", len, [data dataTypeName]] throw];
        return [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:nthTail argCount:3 name:@"nth-tail/3"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"nth-tail" moduleName:[Const coreModuleName]]];

    #pragma mark first
    /** Returns the first element of the list. If the list is empty, this returns nil. */
    id<JSDataProtocol>(^first)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> elem = [xs first];
        if ([xs isEmpty] || [JSNil isNil:elem]) return [JSNil new];
        id<JSDataProtocol> first = nil;
        if ([JSList isKindOfList:elem]) {
            first = [(JSList *)elem first];
        } else if ([JSString isString:elem]) {
            JSString *str = (JSString *)elem;
            if (![str isEmpty]) first = [[JSString alloc] initWithString:[(JSString *)elem substringFrom:0 count:1]];
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"first/1", @"'sequence'", 1, [elem dataTypeName]] throw];
        }
        return first ? first : [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:first argCount:1 name:@"first/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"first" moduleName:[Const coreModuleName]]];

    #pragma mark rest
    /** Returns a sequence without the first element. If the list is empty, then the list is returned. */
    id<JSDataProtocol>(^rest)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> data = [xs first];
        if ([JSNil isNil:data]) return [JSList new];
        NSMutableArray *rest = nil;
        if ([JSList isKindOfList:data]) {
            rest = [[(JSList *)data value] rest];
            return [JSVector isVector:data] ? [[JSVector alloc] initWithArray:rest] : [[JSList alloc] initWithArray:rest];
        } else if ([JSString isString:data]) {
            NSMutableArray *arr = [Utils stringToArray:(JSString *)data isNative:YES];
            if (![arr isEmpty]) [arr removeObjectAtIndex:0];
            return [[JSVector alloc] initWithArray:arr];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, @"rest/1", @"'sequence'", [data dataTypeName]] throw];
        return [JSList new];
    };
    fn = [[JSFunction alloc] initWithFn:rest argCount:1 name:@"rest/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"rest" moduleName:[Const coreModuleName]]];

    #pragma mark map
    id<JSDataProtocol>(^map)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        NSUInteger innerLen = [xs count] - 1;
        NSUInteger i = 0;
        NSUInteger j = 0;
        JSFunction *fn = [JSFunction dataToFunction:[xs first] position:1 fnName:@"map/n"];
        id<JSDataProtocol> data = [xs second];
        NSMutableArray *second = [Utils toArray:data];
        NSUInteger outerLen = [second count];
        NSMutableArray *acc = [NSMutableArray new];
        NSMutableArray *res = [NSMutableArray new];
        BOOL isList = [JSList isList:data];
        NSMutableArray *elem = nil;
        id<JSDataProtocol> ret = nil;
        NSMutableArray *ast = [xs rest];
        for (i = 0; i < outerLen; i++) { // xs: [[1 2 3] [4 5 6]] => [[1 4] [2 5] [3 6]]  => outerLen: 3, innerLen: 2
            [res removeAllObjects];
            for (j = 0; j < innerLen; j++) {
                elem = [Utils toArray:ast[j] isNative:YES];
                if (!isList) isList = [JSList isList:ast[j]];
                if ([elem count] != outerLen) [[[JSError alloc] initWithFormat:ElementCountWithPositionError, outerLen, [elem count], j] throw];
                [res addObject:[elem nth:i]];
            }
            ret = [fn apply:res];
            [acc addObject:ret];
        }
        return isList ? [[JSList alloc] initWithArray:acc] : [[JSVector alloc] initWithArray:acc];
    };
    fn = [[JSFunction alloc] initWithFn:map argCount:-1 name:@"map/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"map" moduleName:[Const coreModuleName]]];

    #pragma mark conj
    /**
     Takes a vector and n elements, appends the elements to the vector and return resulting new vector. The original vector remains unchanged.
     If a list is given the elements are appended to the head of the list giving a reversed list.

     (conj [1] 2 3 4) ; [1 2 3 4]
     (conj '(1) 2 3 4) ; (4 3 2 1)
     */
    id<JSDataProtocol>(^conj)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        NSMutableArray *list = [[[JSVector dataToList:first position:1 fnName:@"conj/n"] value] mutableCopy];
        NSMutableArray *rest = [xs rest];
        if ([JSVector isVector:first]) {
            [list addObjectsFromArray:rest];
            return [[JSVector alloc] initWithArray:list];
        }
        return [[JSList alloc] initWithArray:[[rest reverse] arrayByAddingObjectsFromArray:list]];
    };
    fn = [[JSFunction alloc] initWithFn:conj argCount:-1 name:@"conj/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"conj" moduleName:[Const coreModuleName]]];

    #pragma mark seq?
    /** Checks if the given element is iteratable, which is a list, vector or string. */
    id<JSDataProtocol>(^seqp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSList isList:first] || [JSVector isVector:first] || [JSString isString:first])];
    };
    fn = [[JSFunction alloc] initWithFn:seqp argCount:1 name:@"seq?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"seq?" moduleName:[Const coreModuleName]]];

    #pragma mark seq
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

    #pragma mark last
    /** Returns the last element of the list. If the list is empty, this returns nil. */
    id<JSDataProtocol>(^last)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> seq = [xs first];
        id<JSDataProtocol> last = nil;
        if ([JSList isKindOfList:seq]) {
            last = [(JSList *)seq last];
        } else if ([JSString isString:seq]) {
            JSString *str = (JSString *)seq;
            NSUInteger len = [str count];
            if (len == 0) return [JSNil new];
            last = [[JSString alloc] initWithString:[(JSString *)seq substringFrom:len - 1 count:1]];
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"last/1", @"'sequence'", 1, [list dataTypeName]] throw];
        }
        return last == nil ? [JSNil new] : last;
    };
    fn = [[JSFunction alloc] initWithFn:last argCount:1 name:@"last/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"last" moduleName:[Const coreModuleName]]];

    #pragma mark drop
    /** Returns the element of the list after removing n elements. */
    id<JSDataProtocol>(^drop)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        JSNumber *num = [JSNumber dataToNumber:first position:1 fnName:@"drop/2"];
        NSInteger n = [num integerValue];
        if ([JSList isList:second]) {
            return [(JSList *)second drop:n];
        } else if ([JSVector isVector:second]) {
            return [(JSVector *)second drop:n];
        } else if ([JSString isString:second]) {
            return [[JSString alloc] initWithString:[(JSString *)second substringFrom:n]];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"drop/2", @"'sequence'", 2, [second dataTypeName]] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:drop argCount:2 name:@"drop/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"drop" moduleName:[Const coreModuleName]]];

    #pragma mark reverse
    /** Returns the reverse of the given list. */
    id<JSDataProtocol>(^reverse)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = [xs first];
        if ([JSList isList:first]) {
            return [(JSList *)first reverse];
        } else if ([JSVector isVector:first]) {
            return [(JSVector *)first reverse];
        } else if ([JSString isString:first]) {
            return [[JSString alloc] initWithString:[(JSString *)first reverse]];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"reverse/1", @"'list' or 'vector'", 1, [first dataTypeName]] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:reverse argCount:1 name:@"reverse/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"reverse" moduleName:[Const coreModuleName]]];

    #pragma mark sort
    /**
     Returns the collection sorted by the given criteria. The first argument depends on the data structure provided. It takes a keyword @c :asc, @c :desc for
     @c list, @vector and @string elements. For @c hash-map, it takes these in a @c vector with first element preferably the sort indicator, @c :key or @c
     value. The first argument can also take a comparison function which returns an integer value, in case of the former data types. For @c hash-map, the
     function is passed as the second element in the @c vector.

     (sort :asc [3 5 2 4])  ; [2 3 4 5]
     (sort :desc '(3 5 2 4))  ; (5 4 3 2)
     (sort :asc "Magic")  ; "Macgi"
     (sort [:key :asc] {:z 2 :a 4 :p -5})  ; [:a :p :z]
     (sort [:value :desc] {:z 2 :a 4 :p -5})  ; [4 2 -5]
     (sort (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1)) ["We" "are" "Legends"])  ; ["Legends" "We" "are"]
     (sort [:value (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1))] {:x "We" :y "are" :z "Legends"})  ; ["Legends" "We" "are"]
     */
    id<JSDataProtocol>(^sort)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2 predicate:ArityPredicateLessThanOrEq];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        NSInteger (*sorter)(id obj1, id obj2, void * context) = sortAscending;
        JSFunction __block *fn = nil;
        NSComparisonResult (^comparator)(id obj1, id obj2) = ^NSComparisonResult(id obj1, id obj2) {
            NSMutableArray *arr = [@[obj1, obj2] mutableCopy];
            return (NSComparisonResult)[(JSNumber *)[fn apply:arr] integerValue];
        };
        if ([JSKeyword isKeyword:first]) {  // (sort :asc [1 2])
            NSString *kwd = [(JSKeyword *)first value];
            if ([kwd isEqual:[Const ascendingKeyword]]) {
                sorter = sortAscending;
            } else if ([kwd isEqual:[Const descendingKeyword]]) {
                sorter = sortDescending;
            }
            if ([JSList isList:second]) {
                return [(JSList *)second sort:sorter];
            } else if ([JSVector isVector:second]) {
                return [(JSVector *)second sort:sorter];
            } else if ([JSString isString:second]) {
                return [(JSString *)second sort:sorter];
            }
        } else if ([JSVector isVector:first]) {  // (sort [:key :asc] {:x 2 :a 4})
            // hash map sorter
            JSHashMap *hm = [JSHashMap dataToHashMap:second position:2 fnName:@"sort/2"];
            BOOL __block isAsc = NO;
            BOOL __block isKey = NO;
            [[(JSVector *)first value] enumerateObjectsUsingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                if ([JSKeyword isKeyword:obj]) {
                    NSString *kwd = [(JSKeyword *)obj value];
                    if ([kwd isEqual:[Const ascendingKeyword]]) {
                        isAsc = YES;
                    } else if ([kwd isEqual:[Const keyKeyword]]) {
                        isKey = YES;
                    }
                } else if ([JSFunction isFunction:obj]) {
                    fn = (JSFunction *)obj;
                }
            }];
            if (isKey) {
                if (fn) return [[JSVector alloc] initWithArray:[hm sortedKeysUsingComparator:comparator]];
                return [[JSVector alloc] initWithArray:[hm sortKeys: isAsc ? sortAscending : sortDescending]];
            }
            if (fn) return [[JSVector alloc] initWithArray:[hm sortedObjectsUsingComparator:comparator]];
            return [[JSVector alloc] initWithArray:[hm sortObjects: isAsc ? sortAscending : sortDescending]];
        } else if ([JSFunction isFunction:first]) {  // (sort (fn (a b) (..)) [4 2 5])
            fn = (JSFunction *)first;
            if ([JSList isList:second]) {
                return [(JSList *)second sortedUsingComparator:comparator];
            } else if ([JSVector isVector:second]) {
                return [(JSVector *)second sortedUsingComparator:comparator];
            } else if ([JSString isString:second]) {
                return [(JSString *)second sortedUsingComparator:comparator];
            }
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"sort/2", @"'sequence' or 'collection'", 2, [second dataTypeName]] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:sort argCount:2 name:@"sort/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"sort" moduleName:[Const coreModuleName]]];

    #pragma mark filter
    /**
     Takes a filter predicate function and a collection, applies the function to each element in the collection and returns the resulting filtered collection.
     */
    id<JSDataProtocol>(^filter)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        JSFunction *fn = [JSFunction dataToFunction:[xs first] position:1 fnName:@"filter/2"];
        id<JSDataProtocol> second = [xs second];
        if ([JSList isList:second]) {
            return [[JSList alloc] initWithArray:[self filterArray:[(JSList *)second value] withPredicate:fn]];
        }
        if ([JSVector isVector:second]) {
            return [[JSVector alloc] initWithArray:[self filterArray:[(JSVector *)second value] withPredicate:fn]];
        }
        if ([JSHashMap isHashMap:second]) {
            return [[JSHashMap alloc] initWithMapTable:[self filterMapTable:[(JSHashMap *)second value] withPredicate:fn]];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"filter/2", @"'collection'", 2, [first dataTypeName]] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:filter argCount:2 name:@"filter/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"filter" moduleName:[Const coreModuleName]]];

    #pragma mark partition
    /**
     Takes a predicate function and a collection, applies the function to each element in the collection and returns the resulting collection partitioned into
     two, where first one satisifies the pedicate and the second does not.
     */
    id<JSDataProtocol>(^parition)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        JSFunction *fn = [JSFunction dataToFunction:[xs first] position:1 fnName:@"partition/2"];
        id<JSDataProtocol> second = [xs second];
        NSArray *ret = nil;
        if ([JSList isList:second]) {
            ret = [self partitionArray:[(JSList *)second value] withPredicate:fn];
            JSList *xs = [JSList new];
            [xs add:[[JSList alloc] initWithArray:[ret first]]];
            [xs add:[[JSList alloc] initWithArray:[ret second]]];
            return xs;
        }
        if ([JSVector isVector:second]) {
            ret = [self partitionArray:[(JSVector *)second value] withPredicate:fn];
            JSVector *vec = [JSVector new];
            [vec add:[[JSVector alloc] initWithArray:[ret first]]];
            [vec add:[[JSVector alloc] initWithArray:[ret second]]];
            return vec;
        }
        if ([JSHashMap isHashMap:second]) {
            ret = [self partitionMapTable:[(JSHashMap *)second value] withPredicate:fn];
            JSVector *vec = [JSVector new];
            [vec add:[[JSHashMap alloc] initWithMapTable:[ret first]]];
            [vec add:[[JSHashMap alloc] initWithMapTable:[ret second]]];
            return vec;
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"partition/2", @"'collection'", 2, [first dataTypeName]] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:parition argCount:2 name:@"partition/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"partition" moduleName:[Const coreModuleName]]];

    #pragma mark flatten
    /** Takes any nested collection and returns its contents as a single collection. */
    id<JSDataProtocol>(^flatten)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        NSMutableArray *acc = [NSMutableArray new];
        if ([JSList isList:first]) {
            return [[JSList alloc] initWithArray:[self flatten:(JSList *)first acc:acc]];
        } else if ([JSVector isVector:first]) {
            return [[JSVector alloc] initWithArray:[self flatten:(JSVector *)first acc:acc]];
        } else if ([JSHashMap isHashMap:first]) {
            return [[JSHashMap alloc] initWithMapTable:[self flattenHashMap:first acc:[NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory
                                                                                                            valueOptions:NSMapTableStrongMemory]]];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, @"flatten/1", @"'collection'", [first dataTypeName]] throw];
        return nil;
    };
    fn = [[JSFunction alloc] initWithFn:flatten argCount:1 name:@"flatten/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"flatten" moduleName:[Const coreModuleName]]];

    #pragma mark take
    id<JSDataProtocol>(^take)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        JSNumber *num = [JSNumber dataToNumber:first position:1 fnName:@"take/2"];
        if ([JSString isString:second]) return [[JSString alloc] initWithString:[(JSString *)second substringFrom:0 count:[num integerValue]]];
        NSMutableArray *list = [[JSVector dataToList:second position:2 fnName:@"take/2"] value];
        NSUInteger n = [num integerValue];
        [TypeUtils checkIndexBounds:list index:n];
        NSMutableArray *res = [[list subarrayWithRange:NSMakeRange(0, n)] mutableCopy];
        if ([JSList isList:second]) return [[JSList alloc] initWithArray:res];
        return [[JSVector alloc] initWithArray:res];
    };
    fn = [[JSFunction alloc] initWithFn:take argCount:2 name:@"take/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"take" moduleName:[Const coreModuleName]]];

    #pragma mark join
    /**
     Take any element and a sequence or collection, joins, the element at each intermediate position, returning a sequence.

     (join "a" [1 2 3])  ; [1 "a" 2 "a" 3]
     (join "bc" "xyz")  ; "xbcybcz"
     (join {:a 1} {:b 2 :c 3})  ; [[:c 3] [[:a 1]] [:b 2]]  For hash-maps order is not maintained
     */
    id<JSDataProtocol>(^join)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        NSString *sep = nil;
        BOOL isString = NO;
        if ([JSHashMap isHashMap:first]) first = [[JSVector alloc] initWithArray:[Utils toArray:first isNative:YES]];
        NSMutableArray *list =  [Utils toArray:second isNative:![JSString isString:second]];
        NSMutableArray *res = [NSMutableArray new];
        NSMutableString *str = [NSMutableString new];
        NSUInteger len = [list count];
        NSUInteger i = 0;
        if ([JSString isString:second]) {
            isString = YES;
            sep = [[JSString dataToString:first fnName:@"join/2"] value];
        }
        for (i = 0; i < len; i++) {
            if (isString) {
                [str appendString:[list nth:i]];
                if (i != len - 1) [str appendString:sep];
            } else {
                [res addObject:[list nth:i]];
                if (i != len - 1) [res addObject:first];
            }
        }
        if (isString) return [[JSString alloc] initWithString:str];
        if ([JSList isList:second]) return [[JSList alloc] initWithArray:res];
        return [[JSVector alloc] initWithArray:res];
    };
    fn = [[JSFunction alloc] initWithFn:join argCount:2 name:@"join/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"join" moduleName:[Const coreModuleName]]];

    #pragma mark zip
    /**
     Takes a list of sequence or collection with equal length and returns a new collection containing sequence with first list containing first elements from
     each sequence, second list containing second elements from each sequence and so on. If a sequence is a list the result is a list as well as the sequence,
     else a vector of sequences. Hash maps are converted to key value pair of vectors.

     (zip '(1 2) [3 4] [5 6])  ; ((1 3 5) [2 4 6])
     (zip [1 2 3] [4 5 6])  ; [[1 4] [2 5] [3 6]]
     (zip "abc" "xyz")  ; ["ax" "by" "cz"]
     (zip {:a 1} {:b 2})  ; [[[:a 1] [:b 2]]]
     */
    id<JSDataProtocol>(^zip)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        NSUInteger innerLen = [xs count];
        NSUInteger i = 0;
        NSUInteger j = 0;
        id<JSDataProtocol> data = [xs first];
        NSMutableArray *first = [Utils toArray:data];
        NSUInteger outerLen = [first count];
        NSMutableArray *res = [NSMutableArray new];
        JSString *str = nil;
        JSVector *sub = nil;
        BOOL isString = [JSString isString:data];
        BOOL isList = [JSList isList:data];
        NSMutableArray *elem = nil;
        for (i = 0; i < outerLen; i++) { // xs: [[1 2 3] [4 5 6]] => [[1 4] [2 5] [3 6]]  => outerLen: 3, innerLen: 2
            isString ? (str = [[JSString alloc] initWithMutableString]) : (sub = [JSVector new]);
            for (j = 0; j < innerLen; j++) {
                elem = [Utils toArray:xs[j] isNative:YES];
                if (!isList) isList = [JSList isList:xs[j]];
                if ([elem count] != outerLen) [[[JSError alloc] initWithFormat:ElementCountWithPositionError, outerLen, [elem count], j] throw];
                if (isString) {
                    id<JSDataProtocol> obj = [elem nth:i];
                    if ([JSNumber isNumber:obj]) {
                        [str appendString:[NSString stringWithFormat:@"%ld", [(JSNumber *)obj integerValue]]];
                    } else {
                        [str appendString:[NSString stringWithFormat:@"%@", obj]];
                    }
                } else {
                    [sub add:[elem nth:i]];
                }
            }
            isString ? [res addObject:str] : isList ? [res addObject:[sub list]] : [res addObject:sub];
        }
        return isList ? [[JSList alloc] initWithArray:res] : [[JSVector alloc] initWithArray:res];
    };
    fn = [[JSFunction alloc] initWithFn:zip argCount:-1 name:@"zip/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"zip" moduleName:[Const coreModuleName]]];

    #pragma mark into
    id<JSDataProtocol>(^into)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        if ([JSNil isNil:second]) return first;
        id<JSDataProtocol> ret = nil;
        if ([JSList isList:first]) {
            JSList *list = (JSList *)first;
            if ([JSList isKindOfList:second]) {
                ret = [Utils addObjectsToList:list fromList:second];
            } else if ([JSString isString:second]) {
                ret = [list addObject:second];
            } else if ([JSHashMap isHashMap:second]) {
                ret = [Utils addObjectsToList:list fromHashMap:second];
            } else {
                [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, @"into/2", @"'sequence' or 'collection'", [second dataTypeName]] throw];
                return [JSNil new];
            }
        } else if ([JSVector isVector:first]) {
            JSVector *vec = (JSVector *)first;
            if ([JSList isKindOfList:second]) {
                ret = [Utils addObjectsToVector:vec fromList:second];
            } else if ([JSString isString:second]) {
                ret = [vec addObject:second];
            } else if ([JSHashMap isHashMap:second]) {
                ret = [Utils addObjectsToVector:vec fromHashMap:second];
            } else {
                [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, @"into/2", @"'sequence' or 'collection'", [second dataTypeName]] throw];
                return [JSNil new];
            }
        } else if ([JSHashMap isHashMap:first]) {
            JSHashMap *hm = (JSHashMap *)first;
            if ([JSList isKindOfList:second]) {
                ret = [Utils addObjectsToHashMap:hm fromList:second];
            } else if ([JSHashMap isHashMap:second]) {
                ret = [Utils addObjectsToHashMap:hm fromHashMap:second];
            } else {
                [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, @"into/2", @"'collection'", [second dataTypeName]] throw];
                return [JSNil new];
            }
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"into/2", @"'collection'", 1, [first dataTypeName]] throw];
            return [JSNil new];
        }
        return ret;
    };
    fn = [[JSFunction alloc] initWithFn:into argCount:2 name:@"into/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"into" moduleName:[Const coreModuleName]]];

    #pragma mark index-of
    id<JSDataProtocol>(^indexOf)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        NSInteger idx = -1;
        if ([JSList isKindOfList:second]) {
            idx = [[(JSList *)second value] indexOfObject:first];
        } else if ([JSString isString:second]) {
            NSString *str = [[JSString dataToString:first fnName:@"index-of"] value];
            NSRange range = [(NSString *)[(JSString *)second value] rangeOfString:str];
            idx = range.location;
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, @"index-of/2", @"'sequence'", 2, [second dataTypeName]] throw];
        }
        return [[JSNumber alloc] initWithInteger: idx == NSNotFound ? -1 : idx];
    };
    fn = [[JSFunction alloc] initWithFn:indexOf argCount:2 name:@"index-of/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"index-of" moduleName:[Const coreModuleName]]];

    #pragma mark foldl
    id<JSDataProtocol>(^foldl)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [self fold:xs isRight:NO];
    };
    fn = [[JSFunction alloc] initWithFn:foldl argCount:3 name:@"foldl/3"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"foldl" moduleName:[Const coreModuleName]]];

    #pragma mark foldr
    id<JSDataProtocol>(^foldr)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [self fold:xs isRight:YES];
    };
    fn = [[JSFunction alloc] initWithFn:foldr argCount:3 name:@"foldr/3"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"foldr" moduleName:[Const coreModuleName]]];

    #pragma mark append
    /** Takes an element, an index, a sequence and appends the element at that index. */
    id<JSDataProtocol>(^append)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:3];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> data = [xs nth:2];
        NSString *fnName = @"append/3";
        NSInteger index = [[JSNumber dataToNumber:[xs second] position:2 fnName:fnName] integerValue];
        if (index < 0) [[[JSError alloc] initWithFormat:IndexOutOfBounds, index, 0] throw];
        if ([JSString isString:data]) {
            JSString *str = [[JSString alloc] initWithMutableString:[[(JSString *)data value] mutableCopy]];
            NSInteger len = [str count];
            if (index > len) [[[JSError alloc] initWithFormat:IndexOutOfBounds, index, len] throw];
            [str append:first atIndex:index];
            return str;
        } else if ([JSList isKindOfList:data]) {
            NSMutableArray *arr = [[(JSList *)data value] mutableCopy];
            NSInteger len = [arr count];
            if (index > len) [[[JSError alloc] initWithFormat:IndexOutOfBounds, index, len] throw];
            [arr insertObject:first atIndex:index];
            return [JSVector isVector:data] ? [[JSVector alloc] initWithArray:arr] : [[JSList alloc] initWithArray:arr];
        }
        [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'sequence'", 3, [data dataTypeName]] throw];
        return [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:append argCount:3 name:@"append/3"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"append" moduleName:[Const coreModuleName]]];
}

- (id<JSDataProtocol>)fold:(NSMutableArray *)xs isRight:(BOOL)isRight {
    [TypeUtils checkArity:xs arity:3];
    JSFunction *fn = [JSFunction dataToFunction:[xs first] position:1 fnName: isRight ? @"foldr/3" : @"foldl/3"];
    id<JSDataProtocol> acc = [xs second];
    NSMutableArray *arr = [Utils toArray:[xs nth:2]];
    id<JSDataProtocol> elem = nil;
    NSEnumerator *itr = isRight ? [arr reverseObjectEnumerator] : [arr objectEnumerator];
    for (elem in itr) {
        if ([NSMutableArray isMutableArray:elem]) {
            NSMutableArray *params = (NSMutableArray *)elem;
            [params addObject:acc];
            acc = [fn apply:(NSMutableArray *)elem];
        } else {
            acc = [fn apply:[@[[[self delegate] eval:elem], acc] mutableCopy]];
        }
    }
    return acc;
}

- (NSMutableArray *)filterArray:(NSMutableArray *)array withPredicate:(JSFunction *)predicate {
    NSMutableArray *res = [NSMutableArray new];
    NSUInteger len = [array count];
    NSUInteger i = 0;
    JSBool *ret = nil;
    id<JSDataProtocol> elem = nil;
    for (i = 0; i < len; i++) {
        elem = array[i];
        ret = [predicate apply:[@[elem] mutableCopy]];
        if ([ret value]) [res addObject:elem];
    }
    return res;
}

- (NSMapTable *)filterMapTable:(NSMapTable *)table withPredicate:(JSFunction *)predicate {
    NSMapTable *res = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    NSArray *allKeys = [table allKeys];
    NSUInteger len = [allKeys count];
    NSUInteger i = 0;
    JSBool *ret = nil;
    id<JSDataProtocol> key = nil;
    id<JSDataProtocol> obj = nil;
    for (i = 0; i < len; i++) {
        key = allKeys[i];
        obj = [table objectForKey:key];
        ret = [predicate apply:[@[key, obj] mutableCopy]];
        if ([ret value]) [res setObject:obj forKey:key];
    }
    return res;
}

- (NSArray<NSMutableArray *> *)partitionArray:(NSMutableArray *)array withPredicate:(JSFunction *)predicate {
    NSMutableArray *res = [NSMutableArray new];
    NSMutableArray *resFail = [NSMutableArray new];
    NSUInteger len = [array count];
    NSUInteger i = 0;
    JSBool *ret = nil;
    id<JSDataProtocol> elem = nil;
    for (i = 0; i < len; i++) {
        elem = array[i];
        ret = [predicate apply:[@[elem] mutableCopy]];
        [ret value] ? [res addObject:elem] : [resFail addObject:elem];
    }
    return @[res, resFail];
}

- (NSArray<NSMapTable *> *)partitionMapTable:(NSMapTable *)table withPredicate:(JSFunction *)predicate {
    NSMapTable *res = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    NSMapTable *resFail = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    NSArray *allKeys = [table allKeys];
    NSUInteger len = [allKeys count];
    NSUInteger i = 0;
    JSBool *ret = nil;
    id<JSDataProtocol> key = nil;
    id<JSDataProtocol> obj = nil;
    for (i = 0; i < len; i++) {
        key = allKeys[i];
        obj = [table objectForKey:key];
        ret = [predicate apply:[@[key, obj] mutableCopy]];
        [ret value] ? [res setObject:obj forKey:key] : [resFail setObject:obj forKey:key];
    }
    return @[res, resFail];
}

- (NSMutableArray *)flatten:(JSList *)xs acc:(NSMutableArray *)acc {
    if (!acc) acc = [NSMutableArray new];
    NSUInteger len = [xs count];
    NSUInteger i = 0;
    id<JSDataProtocol> elem = nil;
    for (i = 0; i < len; i++) {
        elem = [_delegate eval:[xs nth:i]];
        if ([JSList isKindOfList:elem]) {
            [self flatten:elem acc:acc];
        } else {
            [acc addObject:elem];
        }
    }
    return acc;
}

#pragma mark - Vector

- (void)addVectorFunctions {
    JSFunction *fn = nil;

    #pragma mark vector
    /** Create a vector with the given elements. */
    id<JSDataProtocol>(^vector)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSVector alloc] initWithArray:xs];
    };
    fn = [[JSFunction alloc] initWithFn:vector argCount:-1 name:@"vector/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"vector" moduleName:[Const coreModuleName]]];

    #pragma mark vector?
    /** Checks if the given element is a vector. */
    id<JSDataProtocol>(^vectorp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSVector isVector:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:vectorp argCount:1 name:@"vector?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"vector?" moduleName:[Const coreModuleName]]];
}

#pragma mark - Hash map

- (NSMapTable *)flattenHashMap:(JSHashMap *)hashMap acc:(NSMapTable *)acc {
    if (!acc) acc = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    NSArray *allKeys = [hashMap allKeys];
    id<JSDataProtocol> key = nil;
    id<JSDataProtocol> val = nil;
    for (key in allKeys) {
        val = [hashMap objectForKey:key];
        if ([JSHashMap isHashMap:val]) {
            [self flattenHashMap:val acc:acc];
        } else {
            [acc setObject:val forKey:key];
        }
    }
    return acc;
}

- (void)addHashMapFunctions {
    JSFunction *fn = nil;

    #pragma mark hash-map
    /** Create a hash map with given key value pair. The first element is taken as a key and the next element as its value and so on.*/
    id<JSDataProtocol>(^hashmap)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSHashMap alloc] initWithArray:xs];
    };
    fn = [[JSFunction alloc] initWithFn:hashmap argCount:-1 name:@"hash-map/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"hash-map" moduleName:[Const coreModuleName]]];

    #pragma mark hash-map?
    /** Checks if the given element is a hash map. */
    id<JSDataProtocol>(^hashmapp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSHashMap isHashMap:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:hashmapp argCount:1 name:@"hash-map?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"hash-map?" moduleName:[Const coreModuleName]]];

    #pragma mark assoc
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

    #pragma mark dissoc
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

    #pragma mark get
    /**
     Takes a key, a hash map and returns the value associated with the key if present else nil.

     (get {:a 1 :b 2 :c 3} :b) ; 2
     (get {:a 1 :b 2 :c 3} :v) ; nil
     */
    id<JSDataProtocol>(^get)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> coll = [xs second];
        id<JSDataProtocol> val = nil;
        if ([JSHashMap isHashMap:coll]) {
            JSHashMap *hm = (JSHashMap *)[xs second];
             val = [hm objectForKey:[xs first]];
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, @"get/2",  @"'hash-map'", [coll dataTypeName]] throw];
        }
        return val ? val : [JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:get argCount:2 name:@"get/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"get" moduleName:[Const coreModuleName]]];

    #pragma mark contains?
    /**
     Checks if the given sequence or collection contains the object.

     (contains? :a {:a 1 :b 2 :c 3}) ; true
     (contains? 3 {:a 1 :b 2 :c 3}) ; false
     (contains? :v {:a 1 :b 2 :c 3}) ;false
     */
    id<JSDataProtocol>(^contains)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> elem = [xs first];
        id<JSDataProtocol> data = [xs second];
        BOOL isExists = NO;
        NSString *fnName = @"contains?/2";
        if ([JSHashMap isHashMap:data]) {
            isExists = [(JSHashMap *)data containsKey:elem];
        } else if ([JSList isKindOfList:data]) {
            isExists = [[(JSList *)data value] containsObject:elem];
        } else if ([JSString isString:data]) {
            isExists = [(NSString *)[(JSString *)data value] containsString:[elem description]];
        } else {
            [[[JSError alloc] initWithFormat:DataTypeMismatchWithNameArity, fnName, @"'sequence' or 'collection'", 2, [data dataTypeName]] throw];
        }
        return [[JSBool alloc] initWithBool:isExists];
    };
    fn = [[JSFunction alloc] initWithFn:contains argCount:2 name:@"contains?/2"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"contains?" moduleName:[Const coreModuleName]]];

    #pragma mark keys
    /** Returns a list containing the hash map keys. */
    id<JSDataProtocol>(^keys)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"keys/1"];
        return [[JSList alloc] initWithArray:[first allKeys]];
    };
    fn = [[JSFunction alloc] initWithFn:keys argCount:1 name:@"keys/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"keys" moduleName:[Const coreModuleName]]];

    #pragma mark values
    /** Returns a list containing the hash map values. */
    id<JSDataProtocol>(^values)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first] fnName:@"values/1"];
        return [[JSList alloc] initWithArray:[first allObjects]];
    };
    fn = [[JSFunction alloc] initWithFn:values argCount:1 name:@"values/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"values" moduleName:[Const coreModuleName]]];
}

#pragma mark - Atom

- (void)addAtomFunctions {
    JSFunction *fn = nil;

    #pragma mark atom
    /** Create an atom with the given element as its value. */
    id<JSDataProtocol>(^atom)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSAtom alloc] initWithData:[xs first]];
    };
    fn = [[JSFunction alloc] initWithFn:atom argCount:1 name:@"atom/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"atom" moduleName:[Const coreModuleName]]];

    #pragma mark atom?
    /** Checks if the given element is an atom. */
    id<JSDataProtocol>(^atomp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSAtom isAtom:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:atomp argCount:1 name:@"atom?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"atom?" moduleName:[Const coreModuleName]]];

    #pragma mark deref
    /** Dereferences an atom returning the value it holds. */
    id<JSDataProtocol>(^deref)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return ![JSAtom isAtom:first] ? [JSNil new] : [(JSAtom *)first value];
    };
    fn = [[JSFunction alloc] initWithFn:deref argCount:1 name:@"deref/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"deref" moduleName:[Const coreModuleName]]];

    #pragma mark reset!
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

    #pragma mark swap!
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

#pragma mark - Function

- (void)addInvokeFunctions {
    JSFunction* fn = nil;

    #pragma mark throw
    /** Throws an exception with the given data as its value. */
    id<JSDataProtocol>(^throw)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        @throw [[NSException alloc] initWithName:JSLException reason:JSLException userInfo:@{@"jsdata": (id<JSDataProtocol>)[xs first]}];
    };
    fn = [[JSFunction alloc] initWithFn:throw argCount:1 name:@"throw/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"throw" moduleName:[Const coreModuleName]]];

    #pragma mark apply
    /** Takes a function and a list of arguments, invokes the function with the elements in the list as its arguments. */
    id<JSDataProtocol>(^apply)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSFunction *fn = [JSFunction dataToFunction:[xs first] position:1];
        NSMutableArray *arr = [NSMutableArray new];
        NSMutableArray *rest = [xs rest];
        if ([rest count] == 1 && [JSList isKindOfList:[rest first]]) {
            [arr addObjectsFromArray:[(JSList *)[rest first] value]];
        } else {
            [arr addObjectsFromArray:[xs rest]];
        }
        NSInteger fnArgsCount = [fn argsCount];
        NSInteger paramsCount = [arr count];
        if (fnArgsCount != -1 && fnArgsCount != paramsCount) [[[JSError alloc] initWithFormat:ArityError, fnArgsCount, paramsCount] throw];
        return [fn apply:arr];
    };
    fn = [[JSFunction alloc] initWithFn:apply argCount:-1 name:@"apply/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"apply" moduleName:[Const coreModuleName]]];
}

#pragma mark - Symbol

- (void)addSymbolFunctions {
    JSFunction *fn = nil;

    #pragma mark symbol
    /** Creates a symbol from the given string. */
    id<JSDataProtocol>(^symbol)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = [xs first];
        JSSymbol *sym = nil;
        NSString *name = nil;
        if ([JSFunction isFunction:first]) {
            JSFunction *fn = (JSFunction *)first;
            name = [fn name];
            if ([[fn value] isEmpty]) name = [NSString stringWithFormat:@"*/%ld", [fn argsCount]];
        } else {
            name = [[JSString dataToString:first fnName:@"symbol/1"] value];
        }
        sym = [JSSymbol processName:name];
        if ([sym isQualified]) {
            [sym setModuleName:[sym initialModuleName]];
        } else {
            [sym setInitialModuleName:[Const emptyModuleName]];
            [sym resetModuleName];
        }
        return sym;
    };
    fn = [[JSFunction alloc] initWithFn:symbol argCount:1 name:@"symbol/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"symbol" moduleName:[Const coreModuleName]]];

    #pragma mark symbol?
    /** Checks if the given element is a symbol. */
    id<JSDataProtocol>(^symbolp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSSymbol isSymbol:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:symbolp argCount:1 name:@"symbol?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"symbol?" moduleName:[Const coreModuleName]]];
}

#pragma mark - Keyword

- (void)addKeywordFunctions {
    JSFunction *fn = nil;

    #pragma mark keyword
    /** Create a keyword from the given element. */
    id<JSDataProtocol>(^keyword)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol> )[xs first];
        if ([JSKeyword isKeyword:first]) return first;
        return ([JSString isString:first]) ? [[JSKeyword alloc] initWithString:[(JSString *)first value]] :[JSNil new];
    };
    fn = [[JSFunction alloc] initWithFn:keyword argCount:1 name:@"keyword/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"keyword" moduleName:[Const coreModuleName]]];

    #pragma mark keyword?
    /** Checks if the given element is a keyword. */
    id<JSDataProtocol>(^keywordp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSKeyword isKeyword:first] || ([NSString isString:first] && [JSKeyword isEncodedKeyword:(NSString *)first]))];
    };
    fn = [[JSFunction alloc] initWithFn:keywordp argCount:1 name:@"keyword?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"keyword?" moduleName:[Const coreModuleName]]];
}

#pragma mark - Predicate

- (void)addPredicateFunctions {
    JSFunction *fn = nil;

#pragma mark nil?
    /** Checks if the given element is @c nil. */
    id<JSDataProtocol>(^nilp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSNil isNil:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:nilp argCount:1 name:@"nil?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"nil?" moduleName:[Const coreModuleName]]];

    #pragma mark true?
    /** Checks if the given value is @c true. */
    id<JSDataProtocol>(^truep)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSBool isBool:data] && [(JSBool *)data value] == YES)];
    };
    fn = [[JSFunction alloc] initWithFn:truep argCount:1 name:@"true?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"true?" moduleName:[Const coreModuleName]]];

    #pragma mark false?
    /** Checks if the given element is @c false. */
    id<JSDataProtocol>(^falsep)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSBool isBool:data] && [(JSBool *)data value] == NO)];
    };
    fn = [[JSFunction alloc] initWithFn:falsep argCount:1 name:@"false?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"false?" moduleName:[Const coreModuleName]]];

    #pragma mark string?
    /** Checks if the given element is a @c string. */
    id<JSDataProtocol>(^stringp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSString isString:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:stringp argCount:1 name:@"string?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"string?" moduleName:[Const coreModuleName]]];

    #pragma mark number?
    /** Checks if the given element is a @c number. */
    id<JSDataProtocol>(^numberp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[JSNumber isNumber:[xs first]]];
    };
    fn = [[JSFunction alloc] initWithFn:numberp argCount:1 name:@"number?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"number?" moduleName:[Const coreModuleName]]];

    #pragma mark fn?
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

    #pragma mark macro?
    /** Checks if the given element is a @c macro. */
    id<JSDataProtocol>(^macrop)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSFunction isFunction:first] && [(JSFunction *)first isMacro])];
    };
    fn = [[JSFunction alloc] initWithFn:macrop argCount:1 name:@"macro?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"macro?" moduleName:[Const coreModuleName]]];

    #pragma mark zero?
    /** Checks if the given element is @c zero. */
    id<JSDataProtocol>(^zerop)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        JSNumber *num = [JSNumber dataToNumber:[xs first] fnName:@"zero?/1"];
        return [[JSBool alloc] initWithBool:[num integerValue] == 0];
    };
    fn = [[JSFunction alloc] initWithFn:zerop argCount:1 name:@"zero?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"zero?" moduleName:[Const coreModuleName]]];

    #pragma mark coll?
    /** Checks if the given element is a collectio, which is a list, vector or hash-map. */
    id<JSDataProtocol>(^collp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSList isList:first] || [JSVector isVector:first] || [JSHashMap isHashMap:first])];
    };
    fn = [[JSFunction alloc] initWithFn:collp argCount:1 name:@"coll?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"coll?" moduleName:[Const coreModuleName]]];

    #pragma mark even?
    /** Checks if the given element is @c even. */
    id<JSDataProtocol>(^evenp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        NSString *fnName = @"even?/1";
        JSNumber *num = [JSNumber dataToNumber:[xs first] fnName:fnName];
        if ([num isDouble]) [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'integer'", @"double"] throw];
        return [[JSBool alloc] initWithBool:[num integerValue] % 2 == 0];
    };
    fn = [[JSFunction alloc] initWithFn:evenp argCount:1 name:@"even?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"even?" moduleName:[Const coreModuleName]]];

    #pragma mark odd?
    /** Checks if the given element is @c odd. */
    id<JSDataProtocol>(^oddp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        NSString *fnName = @"odd?/1";
        JSNumber *num = [JSNumber dataToNumber:[xs first] fnName:fnName];
        if ([num isDouble]) [[[JSError alloc] initWithFormat:DataTypeMismatchWithName, fnName, @"'integer'", @"double"] throw];
        return [[JSBool alloc] initWithBool:[num integerValue] % 2 != 0];
    };
    fn = [[JSFunction alloc] initWithFn:oddp argCount:1 name:@"odd?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"odd?" moduleName:[Const coreModuleName]]];
}

#pragma mark - eval

- (NSString *)nameFromObject:(id<JSDataProtocol>)obj {
    NSString *name = @"G";
    if ([JSSymbol isSymbol:obj]) {
        name = [(JSSymbol *)obj value];
    } else if ([JSString isString:obj]) {
        name = [(JSString *)obj value];
    }
    return name;
}

/** Exposes the reader which is used for eval */
- (void)addEvalFunctions {
    Core * __weak weakSelf = self;
    JSFunction *fn = nil;

    #pragma mark read-string
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

    #pragma mark slurp
    /** Read content of a file as string. Should be used for smaller files only. */
    id<JSDataProtocol>(^slurp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSString alloc] initWithContentsOfFile:[[JSString dataToString:[xs first] fnName:@"slurp/1"] value]];
    };
    fn = [[JSFunction alloc] initWithFn:slurp argCount:1 name:@"slurp/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"slurp" moduleName:[Const coreModuleName]]];

    #pragma mark gensym
    id<JSDataProtocol>(^gensym)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        NSString *name = @"G";
        if ([xs count] == 1) {
            name = [self nameFromObject:[xs first]];
        }
        return [[JSSymbol alloc] initWithName:[NSString stringWithFormat:@"%@__%ld", name, [State counter]]];
    };
    fn = [[JSFunction alloc] initWithFn:gensym argCount:-1 name:@"gensym/n"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"gensym" moduleName:[Const coreModuleName]]];
}

#pragma mark - IO

- (void)addIOFunctions {
    Core * __weak weakSelf = self;
    JSFunction *fn = nil;

    #pragma mark readline
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

#pragma mark - Meta

- (void)addMetaFunctions {
    JSFunction *fn = nil;

    #pragma mark with-meta
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

    #pragma mark meta
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

#pragma mark - Misc

- (void)addMiscFunctions {
    JSFunction *fn = nil;

    #pragma mark exit*
    /** Exits the current running process. To be used in REPL only. */
    id<JSDataProtocol>(^exitfn)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        exit(0);
    };
    fn = [[JSFunction alloc] initWithFn:exitfn argCount:0 name:@"exit*/0"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"exit*" moduleName:[Const coreModuleName]]];

    #pragma mark time-ms
    /** Returns the current timestamp in milliseconds. */
    id<JSDataProtocol>(^timems)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:0];
        return [[JSNumber alloc] initWithInteger:[Utils timestamp]];
    };
    fn = [[JSFunction alloc] initWithFn:timems argCount:0 name:@"time-ms/0"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"time-ms" moduleName:[Const coreModuleName]]];

    #pragma mark type
    /** Returns the type of the given element. */
    id<JSDataProtocol>(^type)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArityCount:[xs count] arity:1 fnName:@"type/1"];
        return [[JSString alloc] initWithString:[[xs first] dataTypeName]];
    };
    fn = [[JSFunction alloc] initWithFn:type argCount:1 name:@"type/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"type" moduleName:[Const coreModuleName]]];

    #pragma mark info
    /** Returns details on the given element. */
    id<JSDataProtocol>(^info)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArityCount:[xs count] arity:1 fnName:@"info/1"];
        NSMapTable *hm = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        id<JSDataProtocol> elem = [xs first];
        [hm setObject:[[JSString alloc] initWithString:[elem dataTypeName]] forKey:[[JSKeyword alloc] initWithString:@"type"]];
        [hm setObject:[[JSString alloc] initWithString:[elem moduleName] ? [elem moduleName] : @"*"] forKey:[[JSKeyword alloc] initWithString:@"module"]];
        [hm setObject:[elem meta] forKey:[[JSKeyword alloc] initWithString:@"meta"]];
        [hm setObject:[[JSNumber alloc] initWithInteger:[elem position]] forKey:[[JSKeyword alloc] initWithString:@"position"]];
        if ([JSSymbol isSymbol:elem]) {
            JSSymbol *sym = (JSSymbol *)elem;
            [hm setObject:[[JSString alloc] initWithString:[sym value]] forKey:[[JSKeyword alloc] initWithString:@"name"]];
            [hm setObject:[[JSString alloc] initWithString:[sym fnName]] forKey:[[JSKeyword alloc] initWithString:@"function-name"]];
            [hm setObject:[[JSString alloc] initWithString:[sym initialModuleName] ? [sym initialModuleName] : @"*"] forKey:[[JSKeyword alloc] initWithString:@"initial-module"]];
            [hm setObject:[[JSNumber alloc] initWithInteger:[sym arity]] forKey:[[JSKeyword alloc] initWithString:@"arity"]];
            [hm setObject:[[JSNumber alloc] initWithInteger:[sym initialArity]] forKey:[[JSKeyword alloc] initWithString:@"initial-arity"]];
            [hm setObject:[[JSBool alloc] initWithBool:[sym isFault]] forKey:[[JSKeyword alloc] initWithString:@"fault?"]];
            [hm setObject:[[JSBool alloc] initWithBool:[sym isFunction]] forKey:[[JSKeyword alloc] initWithString:@"function?"]];
            [hm setObject:[[JSBool alloc] initWithBool:[sym isModule]] forKey:[[JSKeyword alloc] initWithString:@"module?"]];
            [hm setObject:[[JSBool alloc] initWithBool:[sym isImported]] forKey:[[JSKeyword alloc] initWithString:@"imported?"]];
            [hm setObject:[[JSBool alloc] initWithBool:[sym isQualified]] forKey:[[JSKeyword alloc] initWithString:@"qualified?"]];
            [hm setObject:[[JSString alloc] initWithString:[sym string]] forKey:[[JSKeyword alloc] initWithString:@"value"]];
        } else if ([JSFunction isFunction:elem]) {
            JSFunction *fn = (JSFunction *)elem;
            [hm setObject:[[JSVector alloc] initWithArray:[fn params]] forKey:[[JSKeyword alloc] initWithString:@"arguments"]];
            [hm setObject:[[JSBool alloc] initWithBool:[fn isMacro]] forKey:[[JSKeyword alloc] initWithString:@"macro?"]];
            [hm setObject:[[JSNumber alloc] initWithInteger:[fn argsCount]] forKey:[[JSKeyword alloc] initWithString:@"arity"]];
            [hm setObject:[[JSString alloc] initWithString:[fn name]] forKey:[[JSKeyword alloc] initWithString:@"name"]];
            [hm setObject:[[JSBool alloc] initWithBool:[fn isImported]] forKey:[[JSKeyword alloc] initWithString:@"imported?"]];
        } else if ([JSList isList:elem]) {
            JSList *xs = (JSList *)elem;
            [hm setObject:[[JSNumber alloc] initWithInteger:[xs count]] forKey:[[JSKeyword alloc] initWithString:@"count"]];
            [hm setObject:[[JSBool alloc] initWithBool:[xs isEmpty]] forKey:[[JSKeyword alloc] initWithString:@"empty?"]];
            [hm setObject:[[JSBool alloc] initWithBool:[elem isMutable]] forKey:[[JSKeyword alloc] initWithString:@"mutable?"]];
        } else if ([JSVector isVector:elem]) {
            JSVector *vec = (JSVector *)elem;
            [hm setObject:[[JSNumber alloc] initWithInteger:[vec count]] forKey:[[JSKeyword alloc] initWithString:@"count"]];
            [hm setObject:[[JSBool alloc] initWithBool:[vec isEmpty]] forKey:[[JSKeyword alloc] initWithString:@"empty?"]];
            [hm setObject:[[JSBool alloc] initWithBool:[elem isMutable]] forKey:[[JSKeyword alloc] initWithString:@"mutable?"]];
        } else if ([JSHashMap isHashMap:elem]) {
            JSHashMap *hm = (JSHashMap *)elem;
            [hm setObject:[[JSNumber alloc] initWithInteger:[hm count]] forKey:[[JSKeyword alloc] initWithString:@"count"]];
            [hm setObject:[[JSBool alloc] initWithBool:[elem isMutable]] forKey:[[JSKeyword alloc] initWithString:@"mutable?"]];
        } else if ([JSKeyword isKeyword:elem]) {
            JSKeyword *kwd = (JSKeyword *)elem;
            [hm setObject:[kwd value] forKey:[[JSKeyword alloc] initWithString:@"name"]];
        } else if ([JSAtom isAtom:elem]) {
            JSAtom *atom = (JSAtom *)elem;
            [hm setObject:[[JSString alloc] initWithString:[[atom value] dataTypeName]] forKey:[[JSKeyword alloc] initWithString:@"value-type"]];
        } else if ([JSNumber isNumber:elem]) {
            JSNumber *num = (JSNumber *)elem;
            [hm setObject:[[JSBool alloc] initWithBool:[num isDouble]] forKey:[[JSKeyword alloc] initWithString:@"double?"]];
        } else if ([JSString isString:elem]) {
            JSString *str = (JSString *)elem;
            [hm setObject:[[JSBool alloc] initWithBool:[elem isMutable]] forKey:[[JSKeyword alloc] initWithString:@"mutable?"]];
            [hm setObject:[str value] forKey:[[JSKeyword alloc] initWithString:@"value"]];
        }
        return [[JSHashMap alloc] initWithMapTable:hm];
    };
    fn = [[JSFunction alloc] initWithFn:info argCount:1 name:@"info/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"info" moduleName:[Const coreModuleName]]];
}

#pragma mark - Module

- (NSMapTable * _Nullable)moduleInfo:(NSString *)moduleName {
    Env *env = [Env forModuleName:moduleName];
    if (!env) {
        [[[JSError alloc] initWithFormat:ModuleNotFound, moduleName] throw];
        return nil;
    }
    NSArray *exportedFns = nil;
    NSArray *importedFns = nil;
    NSArray *internalFns = nil;
    NSMapTable *info = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    if (env) {
        exportedFns = [env exportedFunctions];
        importedFns = [env importedFunctions];
        internalFns = [env internalFunctions];
    }
    [info setObject:[[JSVector alloc] initWithArray:[exportedFns mutableCopy]] forKey:[[JSKeyword alloc] initWithString:[Const exports]]];
    [info setObject:[[JSVector alloc] initWithArray:[importedFns mutableCopy]] forKey:[[JSKeyword alloc] initWithString:[Const imports]]];
    [info setObject:[[JSVector alloc] initWithArray:[internalFns mutableCopy]] forKey:[[JSKeyword alloc] initWithString:[Const internal]]];
    [info setObject:[[JSString alloc] initWithString:moduleName] forKey:[[JSKeyword alloc] initWithString:[Const name]]];
    [info setObject:[[JSString alloc] initWithString:[env moduleDescription]] forKey:[[JSKeyword alloc] initWithString:[Const description]]];
    return info;
}

- (void)addModuleFunctions {
    JSFunction *fn = nil;

    #pragma mark current-module-name
    id<JSDataProtocol>(^currentModuleName)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSString alloc] initWithString:[State currentModuleName]];
    };
    fn = [[JSFunction alloc] initWithFn:currentModuleName argCount:0 name:@"current-module-name/0"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"current-module-name" moduleName:[Const coreModuleName]]];

    #pragma mark module-info
    id<JSDataProtocol>(^moduleInfo)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSString *mod = [JSString dataToString:[xs first] fnName:@"module-info/1"];
        NSString* moduleName = (NSString *)[mod value];
        NSMapTable *fns = [self moduleInfo:moduleName];
        if (!fns) return nil;
        return [[JSHashMap alloc] initWithMapTable:fns];
    };
    fn = [[JSFunction alloc] initWithFn:moduleInfo argCount:1 name:@"module-info/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"module-info" moduleName:[Const coreModuleName]]];

    #pragma mark module-exist?
    id<JSDataProtocol>(^moduleExist)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSString *mod = [JSString dataToString:[xs first] fnName:@"module-exist?/1"];
        NSString* moduleName = (NSString *)[mod value];
        Env *env = [Env forModuleName:moduleName];
        return [[JSBool alloc] initWithBool:(env != nil)];
    };
    fn = [[JSFunction alloc] initWithFn:moduleExist argCount:1 name:@"module-exist?/1"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"module-exist?" moduleName:[Const coreModuleName]]];

    #pragma mark all-modules
    id<JSDataProtocol>(^allModules)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        NSArray *modArr = [[[Env modules] allKeys] sortedArrayUsingFunction:sortAscending context:nil hint:[self allModulesSortHint]];
        [self setAllModulesSortHint:[modArr sortedArrayHint]];
        NSMutableArray *modules = [NSMutableArray new];
        NSString *name = nil;
        for (name in modArr) {
            [modules addObject:[[JSString alloc] initWithString:name]];
        }
        return [[JSVector alloc] initWithArray:modules];
    };
    fn = [[JSFunction alloc] initWithFn:allModules argCount:0 name:@"all-modules/0"];
    [_env setObject:fn forKey:[[JSSymbol alloc] initWithFunction:fn name:@"all-modules" moduleName:[Const coreModuleName]]];
}

#pragma mark - Internal

- (Env *)env {
    return _env;
}

- (NSData * _Nullable)allModulesSortHint {
    return _allModuleSortHint;
}

- (void)setAllModulesSortHint:(NSData *)hint {
    _allModuleSortHint = hint;
}

@end
