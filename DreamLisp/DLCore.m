//
//  DLCore.m
//  DreamLisp
//
//  Created by Jaseem V V on 05/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLCore.h"

static NSString *_description = @"The core module.";

/** Core functions exposed to the environment. */
@implementation DLCore {
    DLEnv *_env;
    DLReader *_reader;
    DLPrinter *_printer;
    id<DLDelegate> __weak _delegate;
    NSData *_allModuleSortHint;
    DLNotificationTable *_notifTable;
}

@synthesize delegate = _delegate;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)bootstrap {
    _env = [[DLEnv alloc] init];
    [_env setModuleName:[DLConst coreModuleName]];
    [_env setModuleDescription:_description];
    [_env setIsUserDefined:NO];
    _reader = [DLReader new];
    _printer = [DLPrinter new];
    _notifTable = DLNotificationTable.shared;
    [self addArithmeticFunctions];
    [self addComparisonFunctions];
    [self addPrintFunctions];
    [self addListFunctions];
    [self addEvalFunctions];
    [self addAtomFunctions];
    [self addInvokeFunctions];
    [self addLazyFunctions];
    [self addStringFunctions];
    [self addPredicateFunctions];
    [self addSymbolFunctions];
    [self addKeywordFunctions];
    [self addVectorFunctions];
    [self addHashMapFunctions];
    [self addIOFunctions];
    [self addNotificationFunctions];
    [self addJSONFunctions];
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
    DLFunction *fn = nil;

    id<DLDataProtocol>(^calc)(NSMutableArray *args, SEL sel) = ^id<DLDataProtocol>(NSMutableArray *args, SEL sel) {
        @autoreleasepool {
            NSDecimalNumber *num = [NSDecimalNumber new];
            NSUInteger len = [args count];
            NSUInteger i = 0;
            BOOL isDouble = NO;
            DLNumber *aNum = nil;
            if (len == 0) {
                [DLTypeUtils checkArity:args arity:1];
            } else if (len == 1) {
                return [DLNumber dataToNumber:args[0]];
            }
            if (len >= 2) {
                aNum = [DLNumber dataToNumber:args[0]];
                if ([aNum isDouble]) isDouble = YES;
                NSDecimalNumber *n = [aNum value];
                if ([n respondsToSelector:sel]) {
                    aNum = [DLNumber dataToNumber:args[1]];
                    if ([aNum isDouble]) isDouble = YES;
                    num = objc_msgSend(n, sel, [aNum value]);
                }
            }
            if (len > 2) {
                for (i = 2; i < len; i++) {
                    aNum = [DLNumber dataToNumber:args[i]];
                    if ([aNum isDouble]) isDouble = YES;
                    num = objc_msgSend(num, sel, [aNum value]);
                }
            }
            if (isDouble) return [[DLNumber alloc] initWithDoubleNumber:num];
            return [[DLNumber alloc] initWithNumber:num];
        }
    };

    #pragma mark add
    DLFunction *add = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        @autoreleasepool {
            if ([args isEmpty]) return [[DLNumber alloc] initWithInt:0];
            return calc(args, @selector(decimalNumberByAdding:));
        }
    } name:@"+/n"];
    [add setArgsCount:-1];
    [_env setObject:add forKey:[[DLSymbol alloc] initWithFunction:add name:@"+" moduleName:[DLConst coreModuleName]]];

    #pragma mark subtract
    DLFunction *subtract = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        @autoreleasepool {
            if ([args count] == 1) {
                DLNumber *n = [DLNumber dataToNumber:args[0]];
                DLNumber *ret = nil;
                if ([n isDouble]) {
                    ret = [[DLNumber alloc] initWithDoubleNumber:[[NSDecimalNumber alloc] initWithDouble:(-1.0 * [n doubleValue])]];
                } else {
                    ret = [[DLNumber alloc] initWithNumber:[[NSDecimalNumber alloc] initWithInteger:(-1 * [n integerValue])]];
                }
                return ret;
            }
            return calc(args, @selector(decimalNumberBySubtracting:));
        }
    } name:@"-/n"];
    [subtract setArgsCount:-1];
    [_env setObject:subtract forKey:[[DLSymbol alloc] initWithFunction:subtract name:@"-" moduleName:[DLConst coreModuleName]]];

    #pragma mark multiply
    DLFunction *multiply = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        @autoreleasepool {
            if ([args isEmpty]) return [[DLNumber alloc] initWithInt:1];
            return calc(args, @selector(decimalNumberByMultiplyingBy:));
        }
    } name:@"*/n"];
    [multiply setArgsCount:-1];
    [_env setObject:multiply forKey:[[DLSymbol alloc] initWithFunction:multiply name:@"*" moduleName:[DLConst coreModuleName]]];

    #pragma mark divide
    DLFunction *divide = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        @autoreleasepool {
            if ([args count] == 1) [args insertObject:[[DLNumber alloc] initWithInteger:1] atIndex:0];
            return calc(args, @selector(decimalNumberByDividingBy:));
        }
    } name:@"//n"];
    [divide setArgsCount:-1];
    [_env setObject:divide forKey:[[DLSymbol alloc] initWithFunction:divide name:@"/" moduleName:[DLConst coreModuleName]]];

    #pragma mark mod
    id<DLDataProtocol>(^mod)(NSMutableArray *args) = ^id<DLDataProtocol>(NSMutableArray *args) {
        @autoreleasepool {
            [DLTypeUtils checkArity:args arity:2];
            DLNumber *lhs = [DLNumber dataToNumber:[args first] fnName:@"mod/2"];
            DLNumber *rhs = [DLNumber dataToNumber:[args second] fnName:@"mod/2"];
            DLNumber *ret = nil;
            if ([lhs isDouble] || [rhs isDouble]) {
                ret = [[DLNumber alloc] initWithDouble:dmod([lhs doubleValue], [rhs doubleValue])];
            } else {
                ret = [[DLNumber alloc] initWithInteger:[lhs integerValue] % [rhs integerValue]];
            }
            return ret;
        }
    };
    fn = [[DLFunction alloc] initWithFn:mod argCount:2 name:@"mod/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"mod" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Comparison

/** Checks if the given data are equal.  */
- (BOOL)isEqual:(id<DLDataProtocol>)lhs rhs:(id<DLDataProtocol>)rhs {
    if ([DLNumber isNumber:lhs] && [DLNumber isNumber:rhs]) {
        return [(DLNumber *)lhs isEqual:(DLNumber *)rhs];
    } else if ([DLSymbol isSymbol:lhs] && [DLSymbol isSymbol:rhs]) {
        return [(DLSymbol *)lhs isEqual:(DLSymbol *)rhs];
    } else if ([DLString isString:lhs] && [DLString isString:rhs]) {
        return [(DLString *)lhs isEqual:(DLString *)rhs];
    } else if ([DLKeyword isKeyword:lhs] && [DLKeyword isKeyword:rhs]) {
        return [(DLKeyword *)lhs isEqual:(DLKeyword *)rhs];
    } else if (([DLList isList:lhs] && [DLList isList:rhs]) ||
               ([DLList isList:lhs] && [DLVector isVector:rhs]) ||
               ([DLVector isVector:lhs] && [DLList isList:rhs]) ||
               ([DLVector isVector:lhs] && [DLVector isVector:rhs])) {
        return [(DLList *)lhs isEqual:(DLList *)rhs];
    } else if ([DLHashMap isHashMap:lhs] && [DLHashMap isHashMap:rhs]) {
        return [(DLHashMap *)lhs isEqual:(DLHashMap *)rhs];
    } else if ([DLNil isNil:lhs] && [DLNil isNil:rhs]) {
        return YES;
    } else if ([DLBool isBool:lhs] && [DLBool isBool:rhs]) {
        return [(DLBool *)lhs isEqual:(DLBool *)rhs];
    }
    return NO;
}

/** Functions that does monotonic comparison. If only one argument is given, @c true is returned. */
- (void)addComparisonFunctions {
    id<DLDataProtocol>(^compare)(NSMutableArray *args, SEL sel) = ^id<DLDataProtocol>(NSMutableArray *args, SEL sel) {
        @autoreleasepool {
            BOOL ret = YES;
            NSUInteger i = 0;
            NSUInteger len = [args count];
            if (len >= 2) {
                for (i = 0; i < len - 1; i++) {
                    NSMethodSignature *methodSig = [[NSDecimalNumber class] instanceMethodSignatureForSelector:sel];
                    NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:methodSig];
                    [invocation setSelector:sel];
                    [invocation setTarget:[(DLNumber *)args[i] value]];
                    NSNumber *num = [(DLNumber *)args[i + 1] value];
                    [invocation setArgument:&num atIndex:2];
                    [invocation invoke];
                    [invocation getReturnValue:&ret];
                    if (!ret) { break; }
                }
            } else if (len == 0) {
                [DLTypeUtils checkArity:args arity:1];
            }
            return [[DLBool alloc] initWithBool:ret];
        }
    };

    #pragma mark <
    DLFunction *lessThan = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isLessThan:));
    } name:@"</n"];
    [lessThan setArgsCount:-1];
    [_env setObject:lessThan forKey:[[DLSymbol alloc] initWithFunction:lessThan name:@"<" moduleName:[DLConst coreModuleName]]];

    #pragma mark >
    DLFunction *greaterThan = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isGreaterThan:));
    } name:@">/n"];
    [greaterThan setArgsCount:-1];
    [_env setObject:greaterThan forKey:[[DLSymbol alloc] initWithFunction:greaterThan name:@">" moduleName:[DLConst coreModuleName]]];

    #pragma mark <=
    DLFunction *lessThanOrEqualTo = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isLessThanOrEqualTo:));
    } name:@"<=/n"];
    [lessThanOrEqualTo setArgsCount:-1];
    [_env setObject:lessThanOrEqualTo forKey:[[DLSymbol alloc] initWithFunction:lessThanOrEqualTo name:@"<=" moduleName:[DLConst coreModuleName]]];

    #pragma mark >=
    DLFunction *greaterThanOrEqualTo = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        return compare(args, @selector(isGreaterThanOrEqualTo:));
    } name:@">=/n"];
    [greaterThanOrEqualTo setArgsCount:-1];
    [_env setObject:greaterThanOrEqualTo forKey:[[DLSymbol alloc] initWithFunction:greaterThanOrEqualTo name:@">=" moduleName:[DLConst coreModuleName]]];

    #pragma mark =
    DLFunction *equalTo = [[DLFunction alloc] initWithFn:^id<DLDataProtocol>(NSMutableArray *args) {
        @autoreleasepool {
            BOOL ret = YES;
            NSUInteger len = [args count];
            NSUInteger i = 0;
            if (len >= 2) {
                for (i = 0; i < len - 1; i++) {
                    ret = [self isEqual:(id<DLDataProtocol>)args[i] rhs:(id<DLDataProtocol>)args[i + 1]];
                    if (!ret) { break; }
                }
            } else if (len == 0) {
                [DLTypeUtils checkArity:args arity:1];
            }
            return [[DLBool alloc] initWithBool:ret];
        }
    } name:@"=/n"];
    [equalTo setArgsCount:-1];
    [_env setObject:equalTo forKey:[[DLSymbol alloc] initWithFunction:equalTo name:@"=" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Print

/** Add various string functions that returns a string or prints it to stdout. */
- (void)addPrintFunctions {
    DLCore * __weak weakSelf = self;
    DLFunction *fn = nil;

    #pragma mark println
    id<DLDataProtocol>(^println)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            NSUInteger len = [xs count];
            NSUInteger i = 0;
            NSMutableArray *ret = [NSMutableArray new];
            for (i = 0; i < len; i++) {
                [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];  // readably false means for viewing in REPL (println "a") -> a nil
            }
            [[this->_delegate ioService] writeOutput:[ret componentsJoinedByString:@" "]];
            return [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:println argCount:-1 name:@"println/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"println" moduleName:[DLConst coreModuleName]]];

    #pragma mark prn
    id<DLDataProtocol>(^prn)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            NSUInteger len = [xs count];
            NSUInteger i = 0;
            NSMutableArray *ret = [NSMutableArray new];
            for (i = 0; i < len; i++) {
                [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:true]];  // prints strings as is with quotes (prn "a") -> "a" nil
            }
            [[this->_delegate ioService] writeOutput:[ret componentsJoinedByString:@" "]];
            return [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:prn argCount:-1 name:@"prn/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"prn" moduleName:[DLConst coreModuleName]]];

    #pragma mark print
    id<DLDataProtocol>(^print)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            NSUInteger len = [xs count];
            NSUInteger i = 0;
            NSMutableArray *ret = [NSMutableArray new];
            for (i = 0; i < len; i++) {
                [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];
            }
            [[this->_delegate ioService] writeOutput:[ret componentsJoinedByString:@" "] terminator:@""];
            return nil;
        }
    };
    // prints to stdout without newline and  without returning nil
    fn = [[DLFunction alloc] initWithFn:print argCount:-1 name:@"print/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"print" moduleName:[DLConst coreModuleName]]];

    #pragma mark pr-str
    id<DLDataProtocol>(^prstr)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            NSUInteger len = [xs count];
            NSUInteger i = 0;
            NSMutableArray *ret = [NSMutableArray new];
            for (i = 0; i < len; i++) {
                [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:true]];
            }
            return [[DLString alloc] initWithString:[ret componentsJoinedByString:@" "]];
        }
    };
    // returns escaped strings (pr-str "a") -> "\"a\""
    fn = [[DLFunction alloc] initWithFn:prstr argCount:-1 name:@"pr-str/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"pr-str" moduleName:[DLConst coreModuleName]]];

    #pragma mark str
    id<DLDataProtocol>(^str)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            NSUInteger len = [xs count];
            NSUInteger i = 0;
            NSMutableArray *ret = [NSMutableArray new];
            for (i = 0; i < len; i++) {
                [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];
            }
            return [[DLString alloc] initWithString:[ret componentsJoinedByString:@""]];
        }
    };
    // returns strings as such (str "a") -> "a"
    fn = [[DLFunction alloc] initWithFn:str argCount:-1 name:@"str/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"str" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - List

- (void)addListFunctions {
    DLFunction *fn = nil;

    #pragma mark list
    /** Create a list from the given elements. */
    id<DLDataProtocol>(^list)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            return [[DLList alloc] initWithArray:xs];
        }
    };
    fn = [[DLFunction alloc] initWithFn:list argCount:-1 name:@"list/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"list" moduleName:[DLConst coreModuleName]]];

    #pragma mark list?
    /** Checks if the given element is a list */
    id<DLDataProtocol>(^listp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[[(id<DLDataProtocol>)[xs first] dataType] isEqual:@"DLList"]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:listp argCount:1 name:@"list?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"list?" moduleName:[DLConst coreModuleName]]];

    #pragma mark empty?
    /** Checks if the given list contains no elements. */
    id<DLDataProtocol>(^emptyp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            BOOL ret = YES;
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            ret = [DLString isString:first] ? [(DLString *)first isEmpty] : [[DLList dataToList:first fnName:@"empty?/1"] isEmpty];
            return [[DLBool alloc] initWithBool:ret];
        }
    };
    fn = [[DLFunction alloc] initWithFn:emptyp argCount:1 name:@"empty?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"empty?" moduleName:[DLConst coreModuleName]]];

    #pragma mark count
    /** Returns the number of elements in the given list. */
    id<DLDataProtocol>(^count)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            if ([DLNil isNil:first]) return [[DLNumber alloc] initWithInteger:0];
            NSUInteger count = 0;
            if ([DLString isString:first]) {
                count = [(DLString *)first count];
            } else if ([DLList isKindOfList:first]) {
                count = [(DLList *)first count];
            } else if ([DLHashMap isHashMap:first]) {
                count = [(DLHashMap *)first count];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"count/1", @"'sequence' or 'collection'", 1,
                  [list dataTypeName]] throw];
            }
            return [[DLNumber alloc] initWithInteger:count];
        }
    };
    fn = [[DLFunction alloc] initWithFn:count argCount:1 name:@"count/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"count" moduleName:[DLConst coreModuleName]]];

    #pragma mark cons
    /**
     Takes any element and adds it to the first of the given list.

     (cons 1 '(2 3 4)) ; (1 2 3 4)
     (cons '(1) '(2 3 4)) ; ((1) 2 3 4)
     */
    id<DLDataProtocol>(^cons)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> data = (id<DLDataProtocol>)[xs second];
            NSMutableArray *arr = [[[DLList dataToList:data position:2 fnName:@"cons/2"] value] mutableCopy];
            [arr insertObject:(id<DLDataProtocol>)[xs first] atIndex:0];
            return [[DLList alloc] initWithArray:arr];
        }
    };
    fn = [[DLFunction alloc] initWithFn:cons argCount:2 name:@"cons/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"cons" moduleName:[DLConst coreModuleName]]];

    #pragma mark concat
    /**
     Takes a list of sequences and combines them into one sequence.

     (concat '(-2 -1 0) '(1 2 3 4)) ; (-2 -1 0 1 2 3 4)
     (concat ["a"] ["b"]) ; ["a" "b"]
     (concat "a" "b") ; "ab"
     (concat "a" [1 2 "b"] "c") ; "a12bc"
     */
    id<DLDataProtocol>(^concat)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            id<DLDataProtocol> first = [xs first];
            BOOL isList = [DLList isList:first];
            id<DLDataProtocol> elem = nil;
            NSUInteger len = [xs count];
            NSUInteger i = 1;
            if ([DLList isKindOfList:first]) {
                DLList *list = [[DLList alloc] initWithArray:[(DLList *)first value]];
                for (i = 1; i < len; i++) {
                    elem = [xs nth:i];
                    if ([DLList isKindOfList:elem]) {
                        if (!isList) isList = [DLList isList:elem];
                        [list addObjectsFromList:(DLList *)elem];
                    } else if ([DLString isString:elem]) {
                        NSMutableArray *arr = [DLUtils toArray:elem isNative:YES];
                        [list addObjectsFromArray:arr];
                    } else {
                        [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"concat/n", @"'sequence'", i + 1, [elem dataTypeName]] throw];
                    }
                }
                return isList ? list : [[DLVector alloc] initWithArray:[list value]];
            } else if ([DLString isString:first]) {
                DLString *str = [DLString mutable];
                [str setMutableValue:[(NSString *)[(DLString *)first value] mutableCopy]];
                for (i = 1; i < len; i++) {
                    elem = [xs nth:i];
                    if ([DLList isKindOfList:elem]) {
                        [DLUtils appendStringFromArray:[(DLList *)elem value] string:str];
                    } else if ([DLString isString:elem]) {
                        [str append:elem];
                    } else {
    //                    [str appendString:[elem description]];
                        [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"concat/n", @"'sequence'", i + 1, [elem dataTypeName]] throw];
                    }
                }
                return str;
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"concat/n", @"'sequence'", 1, [first dataTypeName]] throw];
            }
            return [DLList new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:concat argCount:-1 name:@"concat/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"concat" moduleName:[DLConst coreModuleName]]];

    #pragma mark nth
    /**
     Returns the nth element from the sequence.

     (nth 2 [1 2 3]) ; 3
     (nth 0 [1 2 3]) ; 1
     (nth 3 "abcd") ; "d"
     */
    id<DLDataProtocol>(^nth)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            DLNumber *num = [DLNumber dataToNumber:first position:1 fnName:@"nth/2"];
            NSUInteger n = [num integerValue];
            id <DLDataProtocol> ret = nil;
            if ([DLLazySequence isLazySequence:second]) {
                DLLazySequence *seq = (DLLazySequence *)second;
                NSUInteger index = [num integerValue];
                if (index >= [seq length]) return [DLNil new];
                seq.index = index;
                [seq apply];
                ret = [[seq acc] firstObject];

            } else {
                NSMutableArray *seq = [DLUtils toArray:second isNative:YES];
                [DLTypeUtils checkIndexBounds:seq index:n];
                ret = [seq nth:n];
            }
            return ret ? ret : [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:nth argCount:2 name:@"nth/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"nth" moduleName:[DLConst coreModuleName]]];

    #pragma mark nth-tail
    /** Takes a start, end index, a sequence and returns a sub-sequence within the given indices inclusive. */
    id<DLDataProtocol>(^nthTail)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:3 predicate:ArityPredicateLessThanOrEq];
            id<DLDataProtocol> data = [xs nth:2];
            NSUInteger len = [xs count];
            NSString *fnName = @"nth-tail/3";
            NSInteger start = [[DLNumber dataToNumber:[xs first] position:1 fnName:fnName] integerValue];
            NSInteger end = [[DLNumber dataToNumber:[xs second] position:2 fnName:fnName] integerValue];;
            NSUInteger count = 0;
            if ([DLLazySequence isLazySequence:data]) {
                DLLazySequence *seq = (DLLazySequence *)data;
                if (end >= [seq length]) return [DLNil new];
                seq.index = start;
                NSUInteger idx = start;
                while (idx <= end) {
                    [seq apply];
                    idx++;
                }
                NSMutableArray *ret = [seq acc];
                if (!ret) return [DLNil new];
                if ([seq sequenceType] == SequenceTypeList) return [[DLList alloc] initWithArray:ret];
                if ([seq sequenceType] == SequenceTypeVector) return [[DLVector alloc] initWithArray:ret];
                if ([seq sequenceType] == SequenceTypeString) return [[DLString alloc] initWithArray:ret];
            } else if ([DLString isString:data]) {
                DLString *str = (DLString *)data;
                return [[DLString alloc] initWithString:[str substringFrom:start to:end]];
            }
            NSMutableArray *list = [[DLVector dataToList:data position:len fnName:fnName] value];
            count = [list count];
            [DLTypeUtils checkIndexBoundsCount:count startIndex:start endIndex:end];
            if ([DLList isList:data]) {
                return [[DLList alloc] initWithArray:[(DLList *)data subArrayWithStartIndex:start endIndex:end]];
            } else if ([DLVector isVector:data]) {
                return [[DLVector alloc] initWithArray:[(DLList *)data subArrayWithStartIndex:start endIndex:end]];
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithArity, fnName, @"'sequence'", len, [data dataTypeName]] throw];
            return [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:nthTail argCount:3 name:@"nth-tail/3"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"nth-tail" moduleName:[DLConst coreModuleName]]];

    #pragma mark first
    /** Returns the first element of the list. If the list is empty, this returns nil. */
    id<DLDataProtocol>(^first)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> elem = [xs first];
            if ([xs isEmpty] || [DLNil isNil:elem]) return [DLNil new];
            id<DLDataProtocol> first = nil;
            if ([DLList isKindOfList:elem]) {
                first = [(DLList *)elem first];
            } else if ([DLString isString:elem]) {
                DLString *str = (DLString *)elem;
                if (![str isEmpty]) first = [[DLString alloc] initWithString:[(DLString *)elem substringFrom:0 count:1]];
            } else if ([DLLazySequence isLazySequence:elem]) {
                DLLazySequence *seq = (DLLazySequence *)elem;
                if ([seq hasNext]) {
                    [seq apply];
                }
                id <DLDataProtocol> ret = [[seq acc] first];
                return ret ? ret : [DLNil new];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"first/1", @"'sequence'", 1, [elem dataTypeName]] throw];
            }
            return first ? first : [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:first argCount:1 name:@"first/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"first" moduleName:[DLConst coreModuleName]]];

    #pragma mark rest
    /** Returns a sequence without the first element. If the list is empty, then the list is returned. */
    id<DLDataProtocol>(^rest)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> data = [xs first];
            if ([DLNil isNil:data]) return [DLList new];
            NSMutableArray *rest = nil;
            if ([DLList isKindOfList:data]) {
                rest = [[(DLList *)data value] rest];
                return [DLVector isVector:data] ? [[DLVector alloc] initWithArray:rest] : [[DLList alloc] initWithArray:rest];
            } else if ([DLString isString:data]) {
                NSMutableArray *arr = [DLUtils stringToArray:(DLString *)data isNative:YES];
                if (![arr isEmpty]) [arr removeObjectAtIndex:0];
                return [[DLVector alloc] initWithArray:arr];
            } else if ([DLLazySequence isLazySequence:data]) {
                DLLazySequence *seq = (DLLazySequence *)data;
                if ([seq length] == 0) return [DLNil new];
                seq.index = 1;
                while ([seq hasNext]) {
                    @autoreleasepool {
                        [seq apply];
                    }
                }
                id <DLDataProtocol> ret = nil;
                if ([seq sequenceType] == SequenceTypeList) {
                    ret = [[DLList alloc] initWithArray:[seq acc]];
                } else if ([seq sequenceType] == SequenceTypeVector) {
                    ret = [[DLVector alloc] initWithArray:[seq acc]];
                } else if ([seq sequenceType] == SequenceTypeString) {
                    ret = [[DLString alloc] initWithArray:[seq acc]];
                }
                return ret ? ret : [DLNil new];
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"rest/1", @"'sequence'", [data dataTypeName]] throw];
            return [DLList new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:rest argCount:1 name:@"rest/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"rest" moduleName:[DLConst coreModuleName]]];

    #pragma mark map
    id<DLDataProtocol>(^map)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            DLFunction *fn = [DLFunction dataToFunction:[xs first] position:1];
            id<DLDataProtocol> second = [xs second];
            NSMutableArray *acc = [[NSMutableArray alloc] init];
            NSMutableArray *arr = [(DLList *)second value];
            NSMutableArray *arg = [[NSMutableArray alloc] init];
            id elem = nil;
            for (elem in arr) {
                [arg removeAllObjects];
                [arg addObject:elem];
                [acc addObject:[fn apply:arg]];
            }
            return [DLVector isVector:second] ? [[DLVector alloc] initWithArray:acc]
                                              : [[DLList alloc] initWithArray:acc];
        }
    };
    
    fn = [[DLFunction alloc] initWithFn:map argCount:-1 name:@"map/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"map" moduleName:[DLConst coreModuleName]]];

    #pragma mark conj
    /**
     Takes a vector and n elements, appends the elements to the vector and return resulting new vector. The original vector remains unchanged.
     If a list is given the elements are appended to the head of the list giving a reversed list.

     (conj [1] 2 3 4) ; [1 2 3 4]
     (conj '(1) 2 3 4) ; (4 3 2 1)
     */
    id<DLDataProtocol>(^conj)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            NSMutableArray *list = [[[DLVector dataToList:first position:1 fnName:@"conj/n"] value] mutableCopy];
            NSMutableArray *rest = [xs rest];
            if ([DLVector isVector:first]) {
                [list addObjectsFromArray:rest];
                return [[DLVector alloc] initWithArray:list];
            }
            return [[DLList alloc] initWithArray:[[rest reverse] arrayByAddingObjectsFromArray:list]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:conj argCount:-1 name:@"conj/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"conj" moduleName:[DLConst coreModuleName]]];

    #pragma mark seq?
    /** Checks if the given element is iteratable, which is a list, vector or string. */
    id<DLDataProtocol>(^seqp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            return [[DLBool alloc] initWithBool:([DLList isList:first] || [DLVector isVector:first] || [DLString isString:first])];
        }
    };
    fn = [[DLFunction alloc] initWithFn:seqp argCount:1 name:@"seq?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"seq?" moduleName:[DLConst coreModuleName]]];

    #pragma mark seq
    /**
     Takes a list, vector or a string and returns a list containing individual elements that can be iterated.

     (seq '(1 2 3)) ; (1 2 3)
     (seq [1 2 3]) ; (1 2 3)
     (seq "abc") ; ("a" "b" "c")
     */
    id<DLDataProtocol>(^seq)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            if ([xs count] == 0) return [DLNil new];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            if ([DLList isList:first]) {
                DLList *list = (DLList *)first;
                if ([list count] == 0) return [DLNil new];
                return list;
            }
            if ([DLVector isVector:first]) {
                DLVector *vec = (DLVector *)first;
                if ([vec count] == 0) return [DLNil new];
                return [vec list];
            }
            if ([DLString isString:first]) {
                NSMutableArray *arr = [NSMutableArray new];
                NSUInteger i = 0;
                DLString *str = (DLString *)first;
                NSString *string = [str value];
                NSUInteger len = [string count];
                if (len == 0) return [DLNil new];
                for(i = 0; i < len; i++) {
                    [arr addObject:[[DLString alloc] initWithFormat:@"%c", [string characterAtIndex:i]]];
                }
                return [[DLList alloc] initWithArray:arr];
            }
            if ([DLNil isNil:first]) return (DLNil *)first;
            [[[DLError alloc] initWithDescription:DLSequenceError] throw];
            return nil;
        }
    };
    fn = [[DLFunction alloc] initWithFn:seq argCount:1 name:@"seq/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"seq" moduleName:[DLConst coreModuleName]]];

    #pragma mark last
    /** Returns the last element of the list. If the list is empty, this returns nil. */
    id<DLDataProtocol>(^last)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> seq = [xs first];
            id<DLDataProtocol> last = nil;
            if ([DLList isKindOfList:seq]) {
                last = [(DLList *)seq last];
            } else if ([DLString isString:seq]) {
                DLString *str = (DLString *)seq;
                NSUInteger len = [str count];
                if (len == 0) return [DLNil new];
                last = [[DLString alloc] initWithString:[(DLString *)seq substringFrom:len - 1 count:1]];
            } else if ([DLLazySequence isLazySequence:seq]) {
                DLLazySequence *lseq = (DLLazySequence *)seq;
                NSUInteger len = [lseq length];
                if (len == 0) return [DLNil new];
                lseq.index = len - 1;
                if ([lseq hasNext]) {
                    id <DLDataProtocol> ret = [lseq next];
                    return ret ? ret : [DLNil new];
                }
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"last/1", @"'sequence'", 1, [list dataTypeName]] throw];
            }
            return last == nil ? [DLNil new] : last;
        }
    };
    fn = [[DLFunction alloc] initWithFn:last argCount:1 name:@"last/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"last" moduleName:[DLConst coreModuleName]]];

    #pragma mark drop
    /** Returns the element of the list after removing n elements. */
    id<DLDataProtocol>(^drop)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            DLNumber *num = [DLNumber dataToNumber:first position:1 fnName:@"drop/2"];
            NSInteger n = [num integerValue];
            if ([DLList isList:second]) {
                return [(DLList *)second drop:n];
            } else if ([DLVector isVector:second]) {
                return [(DLVector *)second drop:n];
            } else if ([DLString isString:second]) {
                return [[DLString alloc] initWithString:[(DLString *)second substringFrom:n]];
            } else if ([DLLazySequence isLazySequence:second]) {
                DLLazySequence *seq = (DLLazySequence *)second;
                if (n < [seq length]) {
                    [[seq value] removeObjectsInRange:NSMakeRange(0, n)];
                    [seq updateEnumerator];
                    return seq;
                }
                return seq;
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"drop/2", @"'sequence'", 2, [second dataTypeName]] throw];
            return nil;
        }
    };
    fn = [[DLFunction alloc] initWithFn:drop argCount:2 name:@"drop/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"drop" moduleName:[DLConst coreModuleName]]];

    #pragma mark reverse
    /** Returns the reverse of the given sequence. */
    void (^reverse)(DLLazySequence *seq, NSMutableArray *xs) = ^void(DLLazySequence *seq, NSMutableArray *xs) {
        [[seq acc] addObject:[xs first]];
    };

    id<DLDataProtocol>(^lazyReverse)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = [xs first];
            DLLazySequence *seq = [DLLazySequence new];
            [seq setIsReverseEnumerate:YES];
            [seq addLazyFunction:[[DLLazyFunction alloc] initWithFn:reverse name:@""]];
            if ([DLList isKindOfList:first]) {
                [seq setSequenceType:[DLVector isVector:first] ? SequenceTypeVector : SequenceTypeList];
                [seq setValue:[(DLList *)first value]];
            } else if ([DLString isString:first]) {
                [seq setSequenceType:SequenceTypeString];
                [seq setValue:[DLUtils toArray:first isNative:YES]];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"reverse/1", @"'sequence'", 1, [first dataTypeName]] throw];
            }
            [seq updateEnumerator];
            return seq;
        }
    };
    fn = [[DLFunction alloc] initWithFn:lazyReverse argCount:1 name:@"reverse/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"reverse" moduleName:[DLConst coreModuleName]]];

    #pragma mark sort
    /**
     Returns the collection sorted by the given criteria. The first argument depends on the data structure provided. It takes a keyword @c :asc, @c :desc for
     @c list, @vector and @string elements. For @c hash-map, it takes these in a @c vector with first element preferably the sort indicator, @c :key or @c
     :value. The first argument can also take a comparison function which returns an integer value, in case of the former data types. For @c hash-map, the
     function is passed as the second element in the @c vector.

     (sort :asc [3 5 2 4])  ; [2 3 4 5]
     (sort :desc '(3 5 2 4))  ; (5 4 3 2)
     (sort :asc "Magic")  ; "Macgi"
     (sort [:key :asc] {:z 2 :a 4 :p -5})  ; [:a :p :z]
     (sort [:value :desc] {:z 2 :a 4 :p -5})  ; [4 2 -5]
     (sort (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1)) ["We" "are" "Legends"])  ; ["Legends" "We" "are"]
     (sort [:value (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1))] {:x "We" :y "are" :z "Legends"})  ; ["Legends" "We" "are"]
     */
    id<DLDataProtocol>(^sort)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2 predicate:ArityPredicateLessThanOrEq];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            NSInteger (*sorter)(id obj1, id obj2, void * context) = sortAscending;
            DLFunction __block *fn = nil;
            NSComparisonResult (^comparator)(id obj1, id obj2) = ^NSComparisonResult(id obj1, id obj2) {
                @autoreleasepool {
                    NSMutableArray *arr = [@[obj1, obj2] mutableCopy];
                    return (NSComparisonResult)[(DLNumber *)[fn apply:arr] integerValue];
                }
            };
            if ([DLKeyword isKeyword:first]) {  // (sort :asc [1 2])
                NSString *kwd = [(DLKeyword *)first value];
                if ([kwd isEqual:[DLConst ascendingKeyword]]) {
                    sorter = sortAscending;
                } else if ([kwd isEqual:[DLConst descendingKeyword]]) {
                    sorter = sortDescending;
                }
                if ([DLList isList:second]) {
                    return [(DLList *)second sort:sorter];
                } else if ([DLVector isVector:second]) {
                    return [(DLVector *)second sort:sorter];
                } else if ([DLString isString:second]) {
                    return [(DLString *)second sort:sorter];
                }
            } else if ([DLVector isVector:first]) {  // (sort [:key :asc] {:x 2 :a 4})
                // hash map sorter
                DLHashMap *hm = [DLHashMap dataToHashMap:second position:2 fnName:@"sort/2"];
                BOOL __block isAsc = NO;
                BOOL __block isKey = NO;
                [[(DLVector *)first value] enumerateObjectsUsingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                    @autoreleasepool {
                        if ([DLKeyword isKeyword:obj]) {
                            NSString *kwd = [(DLKeyword *)obj value];
                            if ([kwd isEqual:[DLConst ascendingKeyword]]) {
                                isAsc = YES;
                            } else if ([kwd isEqual:[DLConst keyKeyword]]) {
                                isKey = YES;
                            }
                        } else if ([DLFunction isFunction:obj]) {
                            fn = (DLFunction *)obj;
                        }
                    }
                }];
                if (isKey) {
                    if (fn) return [[DLVector alloc] initWithArray:[hm sortedKeysUsingComparator:comparator]];
                    return [[DLVector alloc] initWithArray:[hm sortKeys: isAsc ? sortAscending : sortDescending]];
                }
                if (fn) return [[DLVector alloc] initWithArray:[hm sortedObjectsUsingComparator:comparator]];
                return [[DLVector alloc] initWithArray:[hm sortObjects: isAsc ? sortAscending : sortDescending]];
            } else if ([DLFunction isFunction:first]) {  // (sort (fn (a b) (..)) [4 2 5])
                fn = (DLFunction *)first;
                if ([DLList isList:second]) {
                    return [(DLList *)second sortedUsingComparator:comparator];
                } else if ([DLVector isVector:second]) {
                    return [(DLVector *)second sortedUsingComparator:comparator];
                } else if ([DLString isString:second]) {
                    return [(DLString *)second sortedUsingComparator:comparator];
                }
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"sort/2", @"'sequence' or 'collection'", 2, [second dataTypeName]] throw];
            return nil;
        }
    };
    fn = [[DLFunction alloc] initWithFn:sort argCount:2 name:@"sort/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"sort" moduleName:[DLConst coreModuleName]]];

    #pragma mark filter
    /**
     Takes a filter predicate function and a collection, applies the function to each element in the collection and returns the resulting filtered collection.
     */
    void (^filter)(DLLazySequence *seq, NSMutableArray *xs) = ^void(DLLazySequence *seq, NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            DLFunction *fn = [DLFunction dataToFunction:[xs first] position:1 fnName:@"filter/2"];
            NSMutableArray *rest = [xs rest];
            id<DLDataProtocol> elem = [rest first];
            DLBool *res = [fn apply:rest];
            if ([res value]) {
                if ([seq sequenceType] == SequenceTypeHashMap) {
                    DLHashMap *acc = [[seq acc] first];
                    if (acc) {
                        DLHashMap *hm = (DLHashMap *)elem;
                        NSArray *allKeys = [hm allKeys];
                        id<DLDataProtocol> key = nil;
                        for (key in allKeys) {
                            [acc setObject:[hm objectForKey:key] forKey:key];
                        }
                    } else {
                        [[seq acc] addObject:elem];
                    }
                } else {
                    [[seq acc] addObject:elem];
                }
            }
        }
    };

    #pragma mark lazy filter
    id<DLDataProtocol>(^lazyFilter)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            DLFunction *fn = [DLFunction dataToFunction:[xs first] position:1 fnName:@"filter/2"];
            id<DLDataProtocol> second = [xs second];
            DLLazySequence *seq = [DLLazySequence new];
            DLLazyFunction *lfn = [[DLLazyFunction alloc] initWithFn:filter name:@""];
            [seq addLazyFunction:lfn fn:fn];
            if ([DLList isList:second]) {
                [seq setSequenceType:SequenceTypeList];
                [seq setValue:[(DLList *)second value]];
            } else if ([DLVector isVector:second]) {
                [seq setSequenceType:SequenceTypeVector];
                [seq setValue:[(DLVector *)second value]];
            } else if ([DLHashMap isHashMap:second]) {
                [seq setSequenceType:SequenceTypeHashMap];
                [seq setValue:[DLUtils hashMapToHashMapArray:second]];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"filter/2", @"'collection'", 2, [first dataTypeName]] throw];
            }
            return seq;
        }
    };
    fn = [[DLFunction alloc] initWithFn:lazyFilter argCount:2 name:@"filter/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"filter" moduleName:[DLConst coreModuleName]]];

    #pragma mark partition
    /**
     Takes a predicate function and a collection, applies the function to each element in the collection and returns the resulting collection partitioned into
     two, where first one satisifies the pedicate and the second does not.
     */
    id<DLDataProtocol>(^parition)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            DLFunction *fn = [DLFunction dataToFunction:[xs first] position:1 fnName:@"partition/2"];
            id<DLDataProtocol> second = [xs second];
            NSArray *ret = nil;
            if ([DLList isList:second]) {
                ret = [self partitionArray:[(DLList *)second value] withPredicate:fn];
                DLList *xs = [DLList new];
                [xs add:[[DLList alloc] initWithArray:[ret first]]];
                [xs add:[[DLList alloc] initWithArray:[ret second]]];
                return xs;
            }
            if ([DLVector isVector:second]) {
                ret = [self partitionArray:[(DLVector *)second value] withPredicate:fn];
                DLVector *vec = [DLVector new];
                [vec add:[[DLVector alloc] initWithArray:[ret first]]];
                [vec add:[[DLVector alloc] initWithArray:[ret second]]];
                return vec;
            }
            if ([DLHashMap isHashMap:second]) {
                ret = [self partitionMapTable:[(DLHashMap *)second value] withPredicate:fn];
                DLVector *vec = [DLVector new];
                [vec add:[[DLHashMap alloc] initWithMapTable:[ret first]]];
                [vec add:[[DLHashMap alloc] initWithMapTable:[ret second]]];
                return vec;
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"partition/2", @"'collection'", 2, [first dataTypeName]] throw];
            return nil;
        }
    };
    fn = [[DLFunction alloc] initWithFn:parition argCount:2 name:@"partition/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"partition" moduleName:[DLConst coreModuleName]]];

    #pragma mark flatten
    /** Takes any nested collection and returns its contents as a single collection. */
    void(^flatten)(DLLazySequence *seq, NSMutableArray *xs) = ^void(DLLazySequence *seq, NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            NSMutableArray *acc = [NSMutableArray new];
            if ([DLList isKindOfList:first]) {
                [self flatten:(DLList *)first acc:acc];
                [[seq acc] addObjectsFromArray:acc];
            } else if ([DLHashMap isHashMap:first]) {
                [[seq acc] addObject:[self flattenHashMap:first acc:[NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory
                                                                                          valueOptions:NSMapTableStrongMemory]]];
            } else {
                [[seq acc] addObject:first];
            }
        }
    };

    #pragma mark lazy flatten
    id<DLDataProtocol>(^lazyFlatten)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            DLLazySequence *seq = [DLLazySequence new];
            [seq addLazyFunction:[[DLLazyFunction alloc] initWithFn:flatten name:@""]];
            if ([DLLazySequence isLazySequence:first]) {
                seq = (DLLazySequence *)first;
                [seq addLazyFunction:[[DLLazyFunction alloc] initWithFn:flatten name:@""]];
            } else if ([DLList isList:first]) {
                [seq setValue:[(DLList *)first value]];
                [seq setSequenceType:SequenceTypeList];
            } else if ([DLVector isVector:first]) {
                [seq setValue:[(DLVector *)first value]];
                [seq setSequenceType:SequenceTypeVector];
            } else if ([DLHashMap isHashMap:first]) {
                return [[DLHashMap alloc] initWithMapTable:[self flattenHashMap:first acc:[NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory
                                                                                                                valueOptions:NSMapTableStrongMemory]]];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"flatten/1", @"'collection'", [first dataTypeName]] throw];
            }
            return seq;
        }
    };
    fn = [[DLFunction alloc] initWithFn:lazyFlatten argCount:1 name:@"flatten/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"flatten" moduleName:[DLConst coreModuleName]]];

    #pragma mark take
    id<DLDataProtocol>(^take)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            DLNumber *num = [DLNumber dataToNumber:first position:1 fnName:@"take/2"];
            if ([DLLazySequence isLazySequence:second]) {
                DLLazySequence *seq = (DLLazySequence *)second;
                NSUInteger i = 0;
                NSUInteger len = [num integerValue];
                for (i = 0; i < len; i++) {
                    if ([seq hasNext]) {
                        [seq apply];
                    }
                }
                if ([seq sequenceType] == SequenceTypeList) {
                    return [[DLList alloc] initWithArray:[seq acc]];
                }
                if ([seq sequenceType] == SequenceTypeVector) {
                    return [[DLVector alloc] initWithArray:[seq acc]];
                }
                return [[DLString alloc] initWithArray:[seq acc]];
            }
            if ([DLString isString:second]) return [[DLString alloc] initWithString:[(DLString *)second substringFrom:0 count:[num integerValue]]];
            NSMutableArray *list = [[DLVector dataToList:second position:2 fnName:@"take/2"] value];
            NSUInteger n = [num integerValue];
            [DLTypeUtils checkIndexBounds:list index:n];
            NSMutableArray *res = [[list subarrayWithRange:NSMakeRange(0, n)] mutableCopy];
            if ([DLList isList:second]) return [[DLList alloc] initWithArray:res];
            return [[DLVector alloc] initWithArray:res];
        }
    };
    fn = [[DLFunction alloc] initWithFn:take argCount:2 name:@"take/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"take" moduleName:[DLConst coreModuleName]]];

    #pragma mark join
    /**
     Take any element and a sequence or collection, joins, the element at each intermediate position, returning a sequence.

     (join "a" [1 2 3])  ; [1 "a" 2 "a" 3]
     (join "bc" "xyz")  ; "xbcybcz"
     (join {:a 1} {:b 2 :c 3})  ; [[:c 3] [[:a 1]] [:b 2]]  For hash-maps order is not maintained
     */
    id<DLDataProtocol>(^join)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            NSString *sep = nil;
            BOOL isString = NO;
            if ([DLHashMap isHashMap:first]) first = [[DLVector alloc] initWithArray:[DLUtils toArray:first isNative:YES]];
            NSMutableArray *list =  [DLUtils toArray:second isNative:![DLString isString:second]];
            NSMutableArray *res = [NSMutableArray new];
            NSMutableString *str = [NSMutableString new];
            NSUInteger len = [list count];
            NSUInteger i = 0;
            if ([DLString isString:second]) {
                isString = YES;
                sep = [[DLString dataToString:first fnName:@"join/2"] value];
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
            if (isString) return [[DLString alloc] initWithString:str];
            if ([DLList isList:second]) return [[DLList alloc] initWithArray:res];
            return [[DLVector alloc] initWithArray:res];
        }
    };
    fn = [[DLFunction alloc] initWithFn:join argCount:2 name:@"join/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"join" moduleName:[DLConst coreModuleName]]];

    #pragma mark into
    id<DLDataProtocol>(^into)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            if ([DLNil isNil:second]) return first;
            id<DLDataProtocol> ret = nil;
            if ([DLList isList:first]) {
                DLList *list = (DLList *)first;
                if ([DLList isKindOfList:second]) {
                    ret = [DLUtils addObjectsToList:list fromList:second];
                } else if ([DLString isString:second]) {
                    ret = [list addObject:second];
                } else if ([DLHashMap isHashMap:second]) {
                    ret = [DLUtils addObjectsToList:list fromHashMap:second];
                } else {
                    [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"into/2", @"'sequence' or 'collection'", [second dataTypeName]] throw];
                    return [DLNil new];
                }
            } else if ([DLVector isVector:first]) {
                DLVector *vec = (DLVector *)first;
                if ([DLList isKindOfList:second]) {
                    ret = [DLUtils addObjectsToVector:vec fromList:second];
                } else if ([DLString isString:second]) {
                    ret = [vec addObject:second];
                } else if ([DLHashMap isHashMap:second]) {
                    ret = [DLUtils addObjectsToVector:vec fromHashMap:second];
                } else {
                    [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"into/2", @"'sequence' or 'collection'", [second dataTypeName]] throw];
                    return [DLNil new];
                }
            } else if ([DLHashMap isHashMap:first]) {
                DLHashMap *hm = (DLHashMap *)first;
                if ([DLList isKindOfList:second]) {
                    ret = [DLUtils addObjectsToHashMap:hm fromList:second];
                } else if ([DLHashMap isHashMap:second]) {
                    ret = [DLUtils addObjectsToHashMap:hm fromHashMap:second];
                } else {
                    [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"into/2", @"'collection'", [second dataTypeName]] throw];
                    return [DLNil new];
                }
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"into/2", @"'collection'", 1, [first dataTypeName]] throw];
                return [DLNil new];
            }
            return ret;
        }
    };
    fn = [[DLFunction alloc] initWithFn:into argCount:2 name:@"into/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"into" moduleName:[DLConst coreModuleName]]];

    #pragma mark index-of
    id<DLDataProtocol>(^indexOf)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> second = [xs second];
            NSInteger idx = -1;
            if ([DLList isKindOfList:second]) {
                idx = [[(DLList *)second value] indexOfObject:first];
            } else if ([DLString isString:second]) {
                NSString *str = [[DLString dataToString:first fnName:@"index-of"] value];
                NSRange range = [(NSString *)[(DLString *)second value] rangeOfString:str];
                idx = range.location;
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, @"index-of/2", @"'sequence'", 2, [second dataTypeName]] throw];
            }
            return [[DLNumber alloc] initWithInteger: idx == NSNotFound ? -1 : idx];
        }
    };
    fn = [[DLFunction alloc] initWithFn:indexOf argCount:2 name:@"index-of/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"index-of" moduleName:[DLConst coreModuleName]]];

    #pragma mark foldl
    id<DLDataProtocol>(^foldl)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        return [self fold:xs isRight:NO];
    };
    fn = [[DLFunction alloc] initWithFn:foldl argCount:3 name:@"foldl/3"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"foldl" moduleName:[DLConst coreModuleName]]];

    #pragma mark foldr
    id<DLDataProtocol>(^foldr)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        return [self fold:xs isRight:YES];
    };
    fn = [[DLFunction alloc] initWithFn:foldr argCount:3 name:@"foldr/3"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"foldr" moduleName:[DLConst coreModuleName]]];

    #pragma mark append
    /** Takes an element, an index, a sequence and appends the element at that index. */
    id<DLDataProtocol>(^append)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:3];
            id<DLDataProtocol> first = [xs first];
            id<DLDataProtocol> data = [xs nth:2];
            NSString *fnName = @"append/3";
            NSInteger index = [[DLNumber dataToNumber:[xs second] position:2 fnName:fnName] integerValue];
            if (index < 0) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, index, 0] throw];
            if ([DLString isString:data]) {
                DLString *str = [[DLString alloc] initWithMutableString:[[(DLString *)data value] mutableCopy]];
                NSInteger len = [str count];
                if (index > len) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, index, len] throw];
                [str append:first atIndex:index];
                return str;
            } else if ([DLList isKindOfList:data]) {
                NSMutableArray *arr = [[(DLList *)data value] mutableCopy];
                NSInteger len = [arr count];
                if (index > len) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, index, len] throw];
                [arr insertObject:first atIndex:index];
                return [DLVector isVector:data] ? [[DLVector alloc] initWithArray:arr] : [[DLList alloc] initWithArray:arr];
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'sequence'", 3, [data dataTypeName]] throw];
            return [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:append argCount:3 name:@"append/3"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"append" moduleName:[DLConst coreModuleName]]];
}

- (id<DLDataProtocol>)fold:(NSMutableArray *)xs isRight:(BOOL)isRight {
    [DLTypeUtils checkArity:xs arity:3];
    DLFunction *fn = [DLFunction dataToFunction:[xs first] position:1 fnName: isRight ? @"foldr/3" : @"foldl/3"];
    id<DLDataProtocol> acc = [xs second];
    NSMutableArray *arr = [DLUtils toArray:[xs nth:2]];
    id<DLDataProtocol> elem = nil;
    NSEnumerator *itr = isRight ? [arr reverseObjectEnumerator] : [arr objectEnumerator];
    NSMutableArray *params = nil;
    NSUInteger *idx = 0;
    for (elem in itr) {
        if ([NSMutableArray isMutableArray:elem]) {
            params = (NSMutableArray *)elem;
            [params addObject:acc];
            acc = [fn apply:params];
        } else {
            acc = [fn apply:[@[[[self delegate] eval:elem], acc] mutableCopy]];
        }
        [params update:acc atIndex:[params count] - 1];
        ++idx;
    }
    return acc;
}

- (NSMutableArray *)filterArray:(NSMutableArray *)array withPredicate:(DLFunction *)predicate {
    @autoreleasepool {
        NSMutableArray *res = [NSMutableArray new];
        NSUInteger len = [array count];
        NSUInteger i = 0;
        DLBool *ret = nil;
        id<DLDataProtocol> elem = nil;
        for (i = 0; i < len; i++) {
            elem = array[i];
            ret = [predicate apply:[@[elem] mutableCopy]];
            if ([ret value]) [res addObject:elem];
        }
        return res;
    }
}

- (NSMapTable *)filterMapTable:(NSMapTable *)table withPredicate:(DLFunction *)predicate {
    @autoreleasepool {
        NSMapTable *res = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        NSArray *allKeys = [table allKeys];
        NSUInteger len = [allKeys count];
        NSUInteger i = 0;
        DLBool *ret = nil;
        id<DLDataProtocol> key = nil;
        id<DLDataProtocol> obj = nil;
        for (i = 0; i < len; i++) {
            key = allKeys[i];
            obj = [table objectForKey:key];
            ret = [predicate apply:[@[key, obj] mutableCopy]];
            if ([ret value]) [res setObject:obj forKey:key];
        }
        return res;
    }
}

- (NSArray<NSMutableArray *> *)partitionArray:(NSMutableArray *)array withPredicate:(DLFunction *)predicate {
    @autoreleasepool {
        NSMutableArray *res = [NSMutableArray new];
        NSMutableArray *resFail = [NSMutableArray new];
        NSUInteger len = [array count];
        NSUInteger i = 0;
        DLBool *ret = nil;
        id<DLDataProtocol> elem = nil;
        for (i = 0; i < len; i++) {
            elem = array[i];
            ret = [predicate apply:[@[elem] mutableCopy]];
            [ret value] ? [res addObject:elem] : [resFail addObject:elem];
        }
        return @[res, resFail];
    }
}

- (NSArray<NSMapTable *> *)partitionMapTable:(NSMapTable *)table withPredicate:(DLFunction *)predicate {
    @autoreleasepool {
        NSMapTable *res = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        NSMapTable *resFail = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        NSArray *allKeys = [table allKeys];
        NSUInteger len = [allKeys count];
        NSUInteger i = 0;
        DLBool *ret = nil;
        id<DLDataProtocol> key = nil;
        id<DLDataProtocol> obj = nil;
        for (i = 0; i < len; i++) {
            key = allKeys[i];
            obj = [table objectForKey:key];
            ret = [predicate apply:[@[key, obj] mutableCopy]];
            [ret value] ? [res setObject:obj forKey:key] : [resFail setObject:obj forKey:key];
        }
        return @[res, resFail];
    }
}

- (NSMutableArray *)flatten:(DLList *)xs acc:(NSMutableArray *)acc {
    @autoreleasepool {
        if (!acc) acc = [NSMutableArray new];
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        id<DLDataProtocol> elem = nil;
        for (i = 0; i < len; i++) {
            elem = [xs nth:i];
            if ([DLList isKindOfList:elem]) {
                [self flatten:elem acc:acc];
            } else {
                [acc addObject:elem];
            }
        }
        return acc;
    }
}

#pragma mark - Vector

- (void)addVectorFunctions {
    DLFunction *fn = nil;

    #pragma mark vector
    /** Create a vector with the given elements. */
    id<DLDataProtocol>(^vector)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            return [[DLVector alloc] initWithArray:xs];
        }
    };
    fn = [[DLFunction alloc] initWithFn:vector argCount:-1 name:@"vector/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"vector" moduleName:[DLConst coreModuleName]]];

    #pragma mark vector?
    /** Checks if the given element is a vector. */
    id<DLDataProtocol>(^vectorp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLVector isVector:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:vectorp argCount:1 name:@"vector?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"vector?" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Hash map

- (NSMapTable *)flattenHashMap:(DLHashMap *)hashMap acc:(NSMapTable *)acc {
    @autoreleasepool {
        if (!acc) acc = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        NSArray *allKeys = [hashMap allKeys];
        id<DLDataProtocol> key = nil;
        id<DLDataProtocol> val = nil;
        for (key in allKeys) {
            val = [hashMap objectForKey:key];
            if ([DLHashMap isHashMap:val]) {
                [self flattenHashMap:val acc:acc];
            } else {
                [acc setObject:val forKey:key];
            }
        }
        return acc;
    }
}

- (void)addHashMapFunctions {
    DLFunction *fn = nil;

    #pragma mark hash-map
    /** Create a hash map with given key value pair. The first element is taken as a key and the next element as its value and so on.*/
    id<DLDataProtocol>(^hashmap)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            return [[DLHashMap alloc] initWithArray:xs];
        }
    };
    fn = [[DLFunction alloc] initWithFn:hashmap argCount:-1 name:@"hash-map/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"hash-map" moduleName:[DLConst coreModuleName]]];

    #pragma mark hash-map?
    /** Checks if the given element is a hash map. */
    id<DLDataProtocol>(^hashmapp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLHashMap isHashMap:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:hashmapp argCount:1 name:@"hash-map?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"hash-map?" moduleName:[DLConst coreModuleName]]];

    #pragma mark assoc
    /**
     Takes a hash map and key value pairs, add them to the hash map and return a resulting new hash map.

     (assoc {:a 1} :b 2 :c 3) ; {:a 1 :c 4 :b 3}
     */
    id<DLDataProtocol>(^assoc)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:3 predicate:ArityPredicateMin];
            [DLTypeUtils checkArity:xs arity:3 predicate:ArityPredicateOdd];
            DLHashMap *first = [DLHashMap dataToHashMap:[xs first] fnName:@"assoc/n"];
            NSMapTable *table = [[first value] mutableCopy];
            NSMapTable *rest = [[[DLHashMap alloc] initWithArray:[xs rest]] value];
            return [[DLHashMap alloc] initWithMapTable:[table assoc:rest]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:assoc argCount:-1 name:@"assoc/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"assoc" moduleName:[DLConst coreModuleName]]];

    #pragma mark dissoc
    /**
     Takes a hash map and keys, removes the value associated for the key if present and returns the resulting new hash map.

     (dissoc {:a 1 :b 2} :b :c) ; {:a 1}
     */
    id<DLDataProtocol>(^dissoc)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLHashMap *first = [DLHashMap dataToHashMap:[xs first] fnName:@"dissoc/n"];
            NSArray *keys = [xs rest];
            NSMapTable *table = [[first value] mutableCopy];
            return [[DLHashMap alloc] initWithMapTable:[table dissoc:keys]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:dissoc argCount:-1 name:@"dissoc/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"dissoc" moduleName:[DLConst coreModuleName]]];

    #pragma mark get
    /**
     Takes a key, a hash map and returns the value associated with the key if present else nil.

     (get :b {:a 1 :b 2 :c 3}) ; 2
     (get :v {:a 1 :b 2 :c 3}) ; nil
     */
    id<DLDataProtocol>(^get)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> coll = [xs second];
            id<DLDataProtocol> val = nil;
            if ([DLHashMap isHashMap:coll]) {
                DLHashMap *hm = (DLHashMap *)[xs second];
                 val = [hm objectForKey:[xs first]];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"get/2",  @"'hash-map'", [coll dataTypeName]] throw];
            }
            return val ? val : [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:get argCount:2 name:@"get/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"get" moduleName:[DLConst coreModuleName]]];

    #pragma mark contains?
    /**
     Checks if the given sequence or collection contains the object.

     (contains? :a {:a 1 :b 2 :c 3}) ; true
     (contains? 3 {:a 1 :b 2 :c 3}) ; false
     (contains? :v {:a 1 :b 2 :c 3}) ;false
     */
    id<DLDataProtocol>(^contains)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> elem = [xs first];
            id<DLDataProtocol> data = [xs second];
            BOOL isExists = NO;
            NSString *fnName = @"contains?/2";
            if ([DLHashMap isHashMap:data]) {
                isExists = [(DLHashMap *)data containsKey:elem];
            } else if ([DLList isKindOfList:data]) {
                isExists = [[(DLList *)data value] containsObject:elem];
            } else if ([DLString isString:data]) {
                isExists = [(NSString *)[(DLString *)data value] containsString:[elem description]];
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'sequence' or 'collection'", 2, [data dataTypeName]] throw];
            }
            return [[DLBool alloc] initWithBool:isExists];
        }
    };
    fn = [[DLFunction alloc] initWithFn:contains argCount:2 name:@"contains?/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"contains?" moduleName:[DLConst coreModuleName]]];

    #pragma mark keys
    /** Returns a list containing the hash map keys. */
    id<DLDataProtocol>(^keys)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLHashMap *first = [DLHashMap dataToHashMap:[xs first] fnName:@"keys/1"];
            return [[DLList alloc] initWithArray:[first allKeys]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:keys argCount:1 name:@"keys/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"keys" moduleName:[DLConst coreModuleName]]];

    #pragma mark values
    /** Returns a list containing the hash map values. */
    id<DLDataProtocol>(^values)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLHashMap *first = [DLHashMap dataToHashMap:[xs first] fnName:@"values/1"];
            return [[DLList alloc] initWithArray:[first allObjects]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:values argCount:1 name:@"values/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"values" moduleName:[DLConst coreModuleName]]];

    #pragma mark keywordize
    /** Keywordize the given hash-map keys which are not a keyword already. */
    id<DLDataProtocol>(^keywordize)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"keywordize/1";
            DLHashMap *hm = [DLHashMap dataToHashMap:[xs first] fnName:fnName];
            NSArray *allKeys = [hm allKeys];
            NSUInteger i = 0;
            NSUInteger len = [allKeys count];
            id<DLDataProtocol> key = nil;
            id<DLDataProtocol> val = nil;
            DLKeyword *kwd = nil;
            DLHashMap *ret = [DLHashMap new];
            for (i = 0; i < len; i++) {
                key = [allKeys objectAtIndex:i];
                val = [hm objectForKey:key];
                if (![DLKeyword isKeyword:key]) {
                    kwd = [[DLKeyword alloc] initWithString:[key description]];
                }
                [ret setObject:val forKey:kwd];
            }
            return ret;
        }
    };
    fn = [[DLFunction alloc] initWithFn:keywordize argCount:1 name:@"keywordize/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"keywordize" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Atom

- (void)addAtomFunctions {
    DLFunction *fn = nil;

    #pragma mark atom
    /** Create an atom with the given element as its value. */
    id<DLDataProtocol>(^atom)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLAtom alloc] initWithData:[xs first]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:atom argCount:1 name:@"atom/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"atom" moduleName:[DLConst coreModuleName]]];

    #pragma mark atom?
    /** Checks if the given element is an atom. */
    id<DLDataProtocol>(^atomp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLAtom isAtom:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:atomp argCount:1 name:@"atom?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"atom?" moduleName:[DLConst coreModuleName]]];

    #pragma mark deref
    /** Dereferences an atom returning the value it holds. */
    id<DLDataProtocol>(^deref)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            return ![DLAtom isAtom:first] ? [DLNil new] : [(DLAtom *)first value];
        }
    };
    fn = [[DLFunction alloc] initWithFn:deref argCount:1 name:@"deref/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"deref" moduleName:[DLConst coreModuleName]]];

    #pragma mark reset!
    /** Mutates the value of an atom to the new given value. */
    id<DLDataProtocol>(^reset)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            DLAtom *atom = [DLAtom dataToAtom:first fnName:@"reset!/2"];
            id<DLDataProtocol>value = (id<DLDataProtocol>)[xs second];
            [atom setValue:value];
            return value;
        }
    };
    fn = [[DLFunction alloc] initWithFn:reset argCount:2 name:@"reset!/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"reset!" moduleName:[DLConst coreModuleName]]];

    #pragma mark swap!
    /** Takes an atom, a function and arguments, applies the function to the arguments and sets the resulting value as the value of the atom. */
    id<DLDataProtocol>(^swap)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            id<DLDataProtocol> second = (id<DLDataProtocol>)[xs second];
            DLAtom *atom = [DLAtom dataToAtom:first position:1 fnName:@"swap!/n"];
            DLFunction *fn = [DLFunction dataToFunction:second position:2];
            NSMutableArray *more = [xs drop:2];
            [more insertObject:[atom value] atIndex:0];
            [atom setValue:[fn fn](more)];
            return [atom value];
        }
    };
    fn = [[DLFunction alloc] initWithFn:swap argCount:-1 name:@"swap!/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"swap!" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Function

- (void)addInvokeFunctions {
    DLFunction* fn = nil;

    #pragma mark throw
    /** Throws an exception with the given data as its value. */
    id<DLDataProtocol>(^throw)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            @throw [[NSException alloc] initWithName:DLException reason:DLException userInfo:@{@"dldata": (id<DLDataProtocol>)[xs first]}];
        }
    };
    fn = [[DLFunction alloc] initWithFn:throw argCount:1 name:@"throw/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"throw" moduleName:[DLConst coreModuleName]]];

    #pragma mark apply
    /** Takes a function and a list of arguments, invokes the function with the elements in the list as its arguments. */
    id<DLDataProtocol>(^apply)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLFunction *fn = [DLFunction dataToFunction:[xs first] position:1];
            NSMutableArray *arr = [NSMutableArray new];
            NSMutableArray *rest = [xs rest];
            if ([rest count] == 1 && [DLList isKindOfList:[rest first]]) {
                [arr addObjectsFromArray:[(DLList *)[rest first] value]];
            } else {
                [arr addObjectsFromArray:[xs rest]];
            }
            NSInteger fnArgsCount = [fn argsCount];
            NSInteger paramsCount = [arr count];
            if (fnArgsCount != -1 && fnArgsCount != paramsCount) [[[DLError alloc] initWithFormat:DLArityError, fnArgsCount, paramsCount] throw];
            return [fn apply:arr];
        }
    };
    fn = [[DLFunction alloc] initWithFn:apply argCount:-1 name:@"apply/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"apply" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Symbol

- (void)addSymbolFunctions {
    DLFunction *fn = nil;

    #pragma mark symbol
    /** Creates a symbol from the given string. */
    id<DLDataProtocol>(^symbol)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = [xs first];
            DLSymbol *sym = nil;
            NSString *name = nil;
            if ([DLFunction isFunction:first]) {
                DLFunction *fn = (DLFunction *)first;
                name = [fn name];
                if ([[fn value] isEmpty]) name = [NSString stringWithFormat:@"*/%ld", [fn argsCount]];
            } else {
                name = [[DLString dataToString:first fnName:@"symbol/1"] value];
            }
            sym = [DLSymbol processName:name];
            if ([sym isQualified]) {
                [sym setModuleName:[sym initialModuleName]];
            } else {
                [sym setInitialModuleName:[DLConst emptyModuleName]];
                [sym resetModuleName];
            }
            return sym;
        }
    };
    fn = [[DLFunction alloc] initWithFn:symbol argCount:1 name:@"symbol/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"symbol" moduleName:[DLConst coreModuleName]]];

    #pragma mark symbol?
    /** Checks if the given element is a symbol. */
    id<DLDataProtocol>(^symbolp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLSymbol isSymbol:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:symbolp argCount:1 name:@"symbol?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"symbol?" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Keyword

- (void)addKeywordFunctions {
    DLFunction *fn = nil;

    #pragma mark keyword
    /** Create a keyword from the given element. */
    id<DLDataProtocol>(^keyword)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol> )[xs first];
            if ([DLKeyword isKeyword:first]) return first;
            return ([DLString isString:first]) ? [[DLKeyword alloc] initWithString:[(DLString *)first value]] :[DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:keyword argCount:1 name:@"keyword/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"keyword" moduleName:[DLConst coreModuleName]]];

    #pragma mark keyword?
    /** Checks if the given element is a keyword. */
    id<DLDataProtocol>(^keywordp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            return [[DLBool alloc] initWithBool:([DLKeyword isKeyword:first] || ([NSString isString:first] && [DLKeyword isEncodedKeyword:(NSString *)first]))];
        }
    };
    fn = [[DLFunction alloc] initWithFn:keywordp argCount:1 name:@"keyword?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"keyword?" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Lazy

- (void)addLazyFunctions {
    DLFunction *fn = nil;

    #pragma mark lazy-seq
    /** Takes a sequence and returns a lazy-sequence object. */
    id<DLDataProtocol>(^lazySeq)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = [xs first];
            if ([DLString isString:first]) {
                return [[DLLazySequence alloc] initWithArray:[DLUtils toArray:first isNative:YES] sequenceType:SequenceTypeString];
            } else if ([DLList isKindOfList:first]) {
                return [[DLLazySequence alloc] initWithArray:[(DLList *)first value]
                                                sequenceType:[DLVector isVector:first] ? SequenceTypeVector : SequenceTypeList];
            }
            [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, @"lazy-seq/1", @"sequence", [first dataTypeName]] throw];
            return [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:lazySeq argCount:1 name:@"lazy-seq/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"lazy-seq" moduleName:[DLConst coreModuleName]]];

    #pragma mark lazy-seq?
    /** Checks if the given element is a lazy sequence. */
    id<DLDataProtocol>(^lazySeqp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLLazySequence isLazySequence:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:lazySeqp argCount:1 name:@"lazy-seq?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"lazy-seq?" moduleName:[DLConst coreModuleName]]];

    #pragma mark has-next?
    /** Returns whether the lazy sequence has elements which are not realised. */
    id<DLDataProtocol>(^hasNext)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLLazySequence *seq = [DLLazySequence dataToLazySequence:[xs first] fnName:@"has-next?/1"];
            return [[DLBool alloc] initWithBool:[seq hasNext]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:hasNext argCount:1 name:@"has-next?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"has-next?" moduleName:[DLConst coreModuleName]]];

    #pragma mark next
    /** Returns the next element in the lazy sequence if present, else throws an exception. */
    id<DLDataProtocol>(^next)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLLazySequence *seq = [DLLazySequence dataToLazySequence:[xs first] fnName:@"next/1"];
            if (![seq hasNext]) [[[DLError alloc] initWithFormat:DLIndexOutOfBounds, [seq index], [seq length]] throw];
            return [seq next];
        }
    };
    fn = [[DLFunction alloc] initWithFn:next argCount:1 name:@"next/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"next" moduleName:[DLConst coreModuleName]]];

    #pragma mark dorun
    /** Returns the next element in the lazy sequence if present, else throws an exception. */
    id<DLDataProtocol>(^doRun)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLLazySequence *seq = [DLLazySequence dataToLazySequence:[xs first] fnName:@"dorun/1"];
            while ([seq hasNext]) {
                @autoreleasepool {
                    [seq apply];
                }
            }
            return [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:doRun argCount:1 name:@"dorun/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"dorun" moduleName:[DLConst coreModuleName]]];

    #pragma mark doall
    /** Applies the function associated with the lazy sequence to the next element if present as required to realise the
     elements in the sequence. */
    id<DLDataProtocol>(^doAll)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = [xs first];
            if ([DLList isKindOfList:first] || [DLString isString:first]) return first;
            DLLazySequence *seq = [DLLazySequence dataToLazySequence:first fnName:@"doall/1"];
            while ([seq hasNext]) {
                @autoreleasepool {
                    [seq apply];
                }
            }
            if ([seq sequenceType] == SequenceTypeList) {
                return [[DLList alloc] initWithArray:[seq acc]];
            } else if ([seq sequenceType] == SequenceTypeVector) {
                return [[DLVector alloc] initWithArray:[seq acc]];
            } else if ([seq sequenceType] == SequenceTypeHashMap) {
                if ([[seq acc] count] == 1) return [[seq acc] first];
                return [[DLVector alloc] initWithArray:[seq acc]];
            }
            return [[DLString alloc] initWithArray:[seq acc]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:doAll argCount:1 name:@"doall/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"doall" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - String

- (void)addStringFunctions {
    DLFunction *fn = nil;

    #pragma mark uppercase
    /** Returns the given string in uppercase */
    id<DLDataProtocol>(^uppercase)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLString *str = [DLString dataToString:[xs first] fnName:@"uppercase/1"];
            return [[DLString alloc] initWithString:[(NSString *)[str value] uppercaseString]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:uppercase argCount:1 name:@"uppercase/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"uppercase" moduleName:[DLConst coreModuleName]]];

    #pragma mark lowercase
    /** Returns the given string in lowercase */
    id<DLDataProtocol>(^lowercase)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLString *str = [DLString dataToString:[xs first] fnName:@"lowercase/1"];
            return [[DLString alloc] initWithString:[(NSString *)[str value] lowercaseString]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:lowercase argCount:1 name:@"lowercase/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"lowercase" moduleName:[DLConst coreModuleName]]];

    #pragma mark substring
    /** Returns the substring from the given string */
    id<DLDataProtocol>(^substring)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:3];
            NSString *fnName = @"substring/3";
            DLNumber *start = [DLNumber dataToNumber:[xs first] position:0 fnName:fnName];
            DLNumber *end = [DLNumber dataToNumber:[xs second] position:1 fnName:fnName];
            DLString *str = [DLString dataToString:[xs nth:2] fnName:fnName];  /* Third arg */
            return [[DLString alloc] initWithString:[str substringFrom:[start integerValue] to:[end integerValue]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:substring argCount:3 name:@"substring/3"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"substring" moduleName:[DLConst coreModuleName]]];

    #pragma mark regex
    /** Define a compiled regex */
    id<DLDataProtocol>(^regex)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"regex/1";
            DLString *pattern = [DLString dataToString:[xs first] fnName:fnName];
            return [[DLRegex alloc] initWithString:[pattern value]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:regex argCount:1 name:@"regex/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"regex" moduleName:[DLConst coreModuleName]]];

    #pragma mark match
    /**
     Searches whether the given string matches with the given pattern and returns the matches if found or nil.
     (match "Emily Bronte" "[a-zA-Z]+")
     */
    id<DLDataProtocol>(^match)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            NSString *fnName = @"match/2";
            DLString *aStr = [DLString dataToString:[xs first] fnName:fnName];
            NSString *string = [aStr value];
            id<DLDataProtocol> pattn = [xs second];
            DLRegex *regex = nil;
            if ([DLString isString:pattn]) {
                regex = [[DLRegex alloc] initWithString:[(DLString *)pattn value]];
            } else if ([DLRegex isRegex:pattn]) {
                regex = (DLRegex *)pattn;
            } else {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithNameArity, fnName, @"'string' or 'regex'", 1, [pattn dataTypeName]] throw];
            }
            NSArray *matches = [DLUtils matchesInString:string withExpression:[regex value]];
            NSTextCheckingResult *match = nil;
            NSUInteger numRanges = 0;
            NSUInteger num = 0;
            DLVector *ret = [DLVector new];
            for (match in matches) {
                @autoreleasepool {
                    numRanges = [match numberOfRanges];
                    DLVector *vec = [DLVector new];
                    for (num = 0; num < numRanges; num++) {
                        [vec appendObject:[[DLString alloc] initWithString:[string substringWithRange:[match rangeAtIndex:num]]]];
                    }
                    [ret appendObject:vec];
                }
            }
            return ret;
        }
    };
    fn = [[DLFunction alloc] initWithFn:match argCount:2 name:@"match/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"match" moduleName:[DLConst coreModuleName]]];

    #pragma mark split
    /** Split the given string by the path component */
    id<DLDataProtocol>(^split)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            NSString *fnName = @"split/2";
            DLString *aString = [DLString dataToString:[xs first] fnName:fnName];
            DLString *pathComp = [DLString dataToString:[xs second] fnName:fnName];
            NSString *string = [aString value];
            NSArray *arr = [string componentsSeparatedByString:[pathComp value]];
            return [DLUtils convertFromFoundationTypeToDLType:arr];  /* DLVector */
        }
    };
    fn = [[DLFunction alloc] initWithFn:split argCount:2 name:@"split/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"split" moduleName:[DLConst coreModuleName]]];

    #pragma mark trim
    /** Removes spaces from start and end of the given string */
    id<DLDataProtocol>(^trim)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"trim/1";
            DLString *aString = [DLString dataToString:[xs first] fnName:fnName];
            return [[DLString alloc] initWithString:[(NSString *)[aString value] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:trim argCount:1 name:@"trim/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"trim" moduleName:[DLConst coreModuleName]]];

    #pragma mark trim
    /**
     Replaces all occurrences of the search string with the target string in the given string and returns the newly obtained string.
     (replace "123" "+" "he123llo wo123rld")
     */
    id<DLDataProtocol>(^replace)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:3];
            NSString *fnName = @"replace/3";
            DLString *matchString = [DLString dataToString:[xs first] position:0 fnName:fnName];
            DLString *replaceString = [DLString dataToString:[xs second] position:1 fnName:fnName];
            DLString *aString = [DLString dataToString:[xs nth:2] position:2 fnName:fnName];
            NSString *string = [aString value];
            return [[DLString alloc] initWithString:[string stringByReplacingOccurrencesOfString:[matchString value] withString:[replaceString value]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:replace argCount:3 name:@"replace/3"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"replace" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Predicate

- (void)addPredicateFunctions {
    DLFunction *fn = nil;

    #pragma mark nil?
    /** Checks if the given element is @c nil. */
    id<DLDataProtocol>(^nilp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLNil isNil:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:nilp argCount:1 name:@"nil?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"nil?" moduleName:[DLConst coreModuleName]]];

    #pragma mark true?
    /** Checks if the given value is @c true. */
    id<DLDataProtocol>(^truep)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> data = (id<DLDataProtocol>)[xs first];
            return [[DLBool alloc] initWithBool:([DLBool isBool:data] && [(DLBool *)data value] == YES)];
        }
    };
    fn = [[DLFunction alloc] initWithFn:truep argCount:1 name:@"true?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"true?" moduleName:[DLConst coreModuleName]]];

    #pragma mark false?
    /** Checks if the given element is @c false. */
    id<DLDataProtocol>(^falsep)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> data = (id<DLDataProtocol>)[xs first];
            return [[DLBool alloc] initWithBool:([DLBool isBool:data] && [(DLBool *)data value] == NO)];
        }
    };
    fn = [[DLFunction alloc] initWithFn:falsep argCount:1 name:@"false?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"false?" moduleName:[DLConst coreModuleName]]];

    #pragma mark string?
    /** Checks if the given element is a @c string. */
    id<DLDataProtocol>(^stringp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLString isString:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:stringp argCount:1 name:@"string?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"string?" moduleName:[DLConst coreModuleName]]];

    #pragma mark number?
    /** Checks if the given element is a @c number. */
    id<DLDataProtocol>(^numberp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLBool alloc] initWithBool:[DLNumber isNumber:[xs first]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:numberp argCount:1 name:@"number?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"number?" moduleName:[DLConst coreModuleName]]];

    #pragma mark fn?
    /** Checks if the given element is a @c function. */
    id<DLDataProtocol>(^fnp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            if ([DLFunction isFunction:first]) {
                DLFunction *fn = (DLFunction *)first;
                if (![fn isMacro]) return [[DLBool alloc] initWithBool:YES];
            }
            return [[DLBool alloc] initWithBool:NO];
        }
    };
    fn = [[DLFunction alloc] initWithFn:fnp argCount:1 name:@"fn?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"fn?" moduleName:[DLConst coreModuleName]]];

    #pragma mark macro?
    /** Checks if the given element is a @c macro. */
    id<DLDataProtocol>(^macrop)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            return [[DLBool alloc] initWithBool:([DLFunction isFunction:first] && [(DLFunction *)first isMacro])];
        }
    };
    fn = [[DLFunction alloc] initWithFn:macrop argCount:1 name:@"macro?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"macro?" moduleName:[DLConst coreModuleName]]];

    #pragma mark zero?
    /** Checks if the given element is @c zero. */
    id<DLDataProtocol>(^zerop)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLNumber *num = [DLNumber dataToNumber:[xs first] fnName:@"zero?/1"];
            return [[DLBool alloc] initWithBool:[num integerValue] == 0];
        }
    };
    fn = [[DLFunction alloc] initWithFn:zerop argCount:1 name:@"zero?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"zero?" moduleName:[DLConst coreModuleName]]];

    #pragma mark coll?
    /** Checks if the given element is a collectio, which is a list, vector or hash-map. */
    id<DLDataProtocol>(^collp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            return [[DLBool alloc] initWithBool:([DLList isList:first] || [DLVector isVector:first] || [DLHashMap isHashMap:first])];
        }
    };
    fn = [[DLFunction alloc] initWithFn:collp argCount:1 name:@"coll?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"coll?" moduleName:[DLConst coreModuleName]]];

    #pragma mark even?
    /** Checks if the given element is @c even. */
    id<DLDataProtocol>(^evenp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"even?/1";
            DLNumber *num = [DLNumber dataToNumber:[xs first] fnName:fnName];
            if ([num isDouble]) [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'integer'", @"double"] throw];
            return [[DLBool alloc] initWithBool:[num integerValue] % 2 == 0];
        }
    };
    fn = [[DLFunction alloc] initWithFn:evenp argCount:1 name:@"even?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"even?" moduleName:[DLConst coreModuleName]]];

    #pragma mark odd?
    /** Checks if the given element is @c odd. */
    id<DLDataProtocol>(^oddp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"odd?/1";
            DLNumber *num = [DLNumber dataToNumber:[xs first] fnName:fnName];
            if ([num isDouble]) [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithName, fnName, @"'integer'", @"double"] throw];
            return [[DLBool alloc] initWithBool:[num integerValue] % 2 != 0];
        }
    };
    fn = [[DLFunction alloc] initWithFn:oddp argCount:1 name:@"odd?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"odd?" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - eval

- (NSString *)nameFromObject:(id<DLDataProtocol>)obj {
    NSString *name = @"G";
    if ([DLSymbol isSymbol:obj]) {
        name = [(DLSymbol *)obj value];
    } else if ([DLString isString:obj]) {
        name = [(DLString *)obj value];
    }
    return name;
}

/** Exposes the reader which is used for eval */
- (void)addEvalFunctions {
    DLCore * __weak weakSelf = self;
    DLFunction *fn = nil;

    #pragma mark read-string
    /** Takes an expression in string form, tokenizes it returning the expression. */
    id<DLDataProtocol>(^readString)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLCore *this = weakSelf;
            NSMutableArray *exprs = [this->_reader readString:[[DLString dataToString:[xs first] fnName:@"read-string/1"] value]];
            NSUInteger len = [exprs count];
            return (len == 0) ? [DLNil new] : exprs[len - 1];
        }
    };
    fn = [[DLFunction alloc] initWithFn:readString argCount:1 name:@"read-string/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"read-string" moduleName:[DLConst coreModuleName]]];

    #pragma mark slurp
    /** Read content of a file as string. Should be used for smaller files only. */
    id<DLDataProtocol>(^slurp)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            return [[DLString alloc] initWithContentsOfFile:[[DLString dataToString:[xs first] fnName:@"slurp/1"] value]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:slurp argCount:1 name:@"slurp/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"slurp" moduleName:[DLConst coreModuleName]]];

    #pragma mark gensym
    id<DLDataProtocol>(^gensym)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            NSString *name = @"G";
            if ([xs count] == 1) {
                name = [self nameFromObject:[xs first]];
            }
            return [[DLSymbol alloc] initWithName:[NSString stringWithFormat:@"%@__%ld", name, [DLState counter]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:gensym argCount:-1 name:@"gensym/n"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"gensym" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - IO

- (void)addIOFunctions {
    DLCore * __weak weakSelf = self;
    DLFunction *fn = nil;

    #pragma mark readline
    /** Reads a line from the stdin with the given prompt displayed. */
    id<DLDataProtocol>(^readline)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            DLCore *this = weakSelf;
            return [[DLString alloc] initWithString:[[this->_delegate ioService]
                                                     readInputWithPrompt:[[DLString dataToString:[xs first] fnName:@"readline/1"] value]]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:readline argCount:1 name:@"readline/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"readline" moduleName:[DLConst coreModuleName]]];

    #pragma mark write-file
    /**
     Writes the given string to the given file. If the file does not exists, a new file will be created. If the file exists, its contents will be overwritten.
     (write-file "string data" "/tmp/mytext.txt")
     */
    id<DLDataProtocol>(^writeFile)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        [DLTypeUtils checkArity:xs arity:2];
        DLFileOps *fops = [DLFileOps new];
        NSString *fnName = @"write-file/2";
        DLString *aString = [DLString dataToString:[xs first] position:0 fnName:fnName];
        DLString *filePath = [DLString dataToString:[xs second] position:1 fnName:fnName];
        NSString *path = [filePath value];
        [fops createFileIfNotExist:path];
        [fops openFileForWriting:path];
        [fops write:[aString value]];
        [fops closeFile];
        return [[DLBool alloc] initWithBool:YES];
    };
    fn = [[DLFunction alloc] initWithFn:writeFile argCount:2 name:@"write-file/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"write-file" moduleName:[DLConst coreModuleName]]];

    #pragma mark cwd
    /** Get the current working directory. */
    id<DLDataProtocol>(^currentWorkingDirectory)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:0];
            DLFileOps *fops = [DLFileOps new];
            return [[DLString alloc] initWithString:[fops currentDirectoryPath]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:currentWorkingDirectory argCount:0 name:@"cwd/0"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"cwd" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Notification

- (void)addNotificationFunctions {
    DLCore * __weak weakSelf = self;
    DLFunction *fn = nil;

    #pragma mark add-notification
    /**
     Subscribe to a notification.
     (defun data-did-download (data) ..)
     (add-notification :data-did-download data-did-download/1)
     */
    id<DLDataProtocol>(^addNotification)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            [DLTypeUtils checkArity:xs arity:2];
            NSString *fnName = @"add-notification/2";
            DLKeyword *aNotifKey = [DLKeyword dataToKeyword:[xs first] position:0 fnName:fnName];
            DLFunction *notifFn = [DLFunction dataToFunction:[xs second] position:1 fnName:fnName];
            DLNotificationData *notifData = [DLNotificationData new];
            notifData.notificationKey = aNotifKey;
            notifData.notificationHandler = notifFn;
            [this->_notifTable setNotification:notifData];
            [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(notificationDidReceive:) name:(NSString *)[aNotifKey value] object:nil];
            return [[DLBool alloc] initWithBool:YES];
        }
    };
    fn = [[DLFunction alloc] initWithFn:addNotification argCount:2 name:@"add-notification/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"add-notification" moduleName:[DLConst coreModuleName]]];

    #pragma mark post-notification
    /**
     Send a notification with the given notification key and optional data.
     (post-notification :data-did-download {:status true :data [..]})
     */
    id<DLDataProtocol>(^postNotification)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            NSString *fnName = @"post-notification/2";
            DLKeyword *aNotifKey = [DLKeyword dataToKeyword:[xs first] position:0 fnName:fnName];
            NSUInteger len = [xs count];
            id<DLDataProtocol> data = nil;
            if (len == 2) {
                data = [xs second];
            } else if (len > 2) {
                [[[DLError alloc] initWithFormat:DLArityLessThanOrEqualError, 2, len] throw];
            }
            NSMutableArray *args = [NSMutableArray new];
            if (data) {
                [args addObject:data];
            }
            [[NSNotificationCenter defaultCenter] postNotificationName:[aNotifKey value] object:self
                                                              userInfo:@{@"notifKey": aNotifKey, @"args": args}];
            return [[DLBool alloc] initWithBool:YES];
        }
    };
    fn = [[DLFunction alloc] initWithFn:postNotification argCount:2 name:@"post-notification/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"post-notification" moduleName:[DLConst coreModuleName]]];

    #pragma mark remove-notification
    /**
     Remove a notification subscription if present.
     (remove-notification :data-did-download)
     */
    id<DLDataProtocol>(^removeNotification)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLCore *this = weakSelf;
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"remove-notification/1";
            DLKeyword *aNotifKey = [DLKeyword dataToKeyword:[xs first] position:0 fnName:fnName];
            DLNotificationData *notifData = [this->_notifTable notification:aNotifKey];
            [[NSNotificationCenter defaultCenter] removeObserver:self name:(NSString *)[notifData.notificationKey value] object:nil];
            return [[DLBool alloc] initWithBool:YES];
        }
    };
    fn = [[DLFunction alloc] initWithFn:removeNotification argCount:1 name:@"remove-notification/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"remove-notification" moduleName:[DLConst coreModuleName]]];
}

- (void)notificationDidReceive:(NSNotification *)notif {
    NSDictionary *userInfo = [notif userInfo];
    DLKeyword *notifKey = [userInfo objectForKey:DLConst.keyForNotificationKey];
    /*
     For user defined functions, this reflect the fn args array. For in-built function, we could specify the method signature (for eg: in data-did-download,
     it is kind of like delegates)
     */
    NSMutableArray *args = [userInfo objectForKey:DLConst.keyForNotificationValue];
    DLNotificationData *notifData = [_notifTable notification:notifKey];
    DLFunction *fn = notifData.notificationHandler;
    if (fn) {
        [fn apply:args];
    }
}

#pragma mark - JSON

- (void)addJSONFunctions {
    DLFunction *fn = nil;

    #pragma mark decode-json
    /**
     Decodes the JSON string to a hash-map.
     (decode-json "..")
     */
    id<DLDataProtocol>(^decodeJSON)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"decode-json/1";
            id<DLDataProtocol> elem = [xs first];
            if ([DLString isString:elem]) {
                return [DLUtils decodeJSON:elem];
            }
            DLData *data = [DLData dataToData:elem fnName:fnName];
            return [DLUtils decodeJSONFromData:[data value]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:decodeJSON argCount:1 name:@"decode-json/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"decode-json" moduleName:[DLConst coreModuleName]]];

    #pragma mark encode-json
    /**
     Encodes the given hash-map to json string.
     (encode-json {:first-name "Jane" :last-name "Doe"})
     */
    id<DLDataProtocol>(^encodeJSON)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            NSString *fnName = @"encode-json/1";
            DLHashMap *hm = [DLHashMap dataToHashMap:[xs first] fnName:fnName];
            return [DLUtils encodeJSON:hm];
        }
    };
    fn = [[DLFunction alloc] initWithFn:encodeJSON argCount:1 name:@"encode-json/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"encode-json" moduleName:[DLConst coreModuleName]]];
}

// TODO: DLData related ops

#pragma mark - Meta

- (void)addMetaFunctions {
    DLFunction *fn = nil;

    #pragma mark with-meta
    /**
     Associates the given element with a metadata.

     (def f (with-meta :foo 1)) ; :foo  The keyword :foo has meta as 1
     */
    id<DLDataProtocol>(^withMeta)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:2];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            id<DLDataProtocol> meta = (id<DLDataProtocol>)[xs second];
            if ([DLFunction isFunction:first]) return [[DLFunction alloc] initWithMeta:meta func:(DLFunction *)first];
            if ([DLString isString:first]) return [[DLString alloc] initWithMeta:meta string:(DLString *)first];
            if ([DLKeyword isKeyword:first]) return [[DLKeyword alloc] initWithMeta:meta keyword:(DLKeyword *)first];
            if ([DLSymbol isSymbol:first]) return [[DLSymbol alloc] initWithMeta:meta symbol:(DLSymbol *)first];
            if ([DLHashMap isHashMap:first]) return [[DLHashMap alloc] initWithMeta:meta hashmap:(DLHashMap *)first];
            if ([DLList isList:first]) return [[DLList alloc] initWithMeta:meta list:(DLList *)first];
            if ([DLVector isVector:first]) return [[DLVector alloc] initWithMeta:meta vector:(DLVector *)first];
            if ([DLNumber isNumber:first]) return [[DLNumber alloc] initWithMeta:meta number:(DLNumber *)first];
            if ([DLAtom isAtom:first]) return [[DLAtom alloc] initWithMeta:meta atom:(DLAtom *)first];
            return first;
        }
    };
    fn = [[DLFunction alloc] initWithFn:withMeta argCount:2 name:@"with-meta/2"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"with-meta" moduleName:[DLConst coreModuleName]]];

    #pragma mark meta
    /**
     Return a meta associated with the given element if present or nil.

     (meta (with-meta :foo 1)) ; 1
     */
    id<DLDataProtocol>(^meta)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:1];
            id<DLDataProtocol> first = (id<DLDataProtocol>)[xs first];
            if ([DLFunction isFunction:first]) {
                DLFunction *fn = (DLFunction *)first;
                if ([fn meta]) return [fn meta];
            }
            return [first meta] ? [first meta] : [DLNil new];
        }
    };
    fn = [[DLFunction alloc] initWithFn:meta argCount:1 name:@"meta/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"meta" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Misc

- (void)addMiscFunctions {
    DLFunction *fn = nil;

    #pragma mark exit*
    /** Exits the current running process. To be used in REPL only. */
    id<DLDataProtocol>(^exitfn)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        exit(0);
    };
    fn = [[DLFunction alloc] initWithFn:exitfn argCount:0 name:@"exit*/0"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"exit*" moduleName:[DLConst coreModuleName]]];

    #pragma mark time-ms
    /** Returns the current timestamp in milliseconds. */
    id<DLDataProtocol>(^timems)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArity:xs arity:0];
            return [[DLNumber alloc] initWithInteger:[DLUtils timestamp]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:timems argCount:0 name:@"time-ms/0"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"time-ms" moduleName:[DLConst coreModuleName]]];

    #pragma mark type
    /** Returns the type of the given element. */
    id<DLDataProtocol>(^type)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArityCount:[xs count] arity:1 fnName:@"type/1"];
            return [[DLString alloc] initWithString:[[xs first] dataTypeName]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:type argCount:1 name:@"type/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"type" moduleName:[DLConst coreModuleName]]];

    #pragma mark info
    /** Returns details on the given element. */
    id<DLDataProtocol>(^info)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            [DLTypeUtils checkArityCount:[xs count] arity:1 fnName:@"info/1"];
            NSMapTable *hm = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
            id<DLDataProtocol> elem = [xs first];
            [hm setObject:[[DLString alloc] initWithString:[elem dataTypeName]] forKey:[[DLKeyword alloc] initWithString:@"type"]];
            [hm setObject:[[DLString alloc] initWithString:[elem moduleName] ? [elem moduleName] : @"*"] forKey:[[DLKeyword alloc] initWithString:@"module"]];
            [hm setObject:[elem meta] forKey:[[DLKeyword alloc] initWithString:@"meta"]];
            [hm setObject:[[DLNumber alloc] initWithInteger:[elem position]] forKey:[[DLKeyword alloc] initWithString:@"position"]];
            if ([DLSymbol isSymbol:elem]) {
                DLSymbol *sym = (DLSymbol *)elem;
                [hm setObject:[[DLString alloc] initWithString:[sym value]] forKey:[[DLKeyword alloc] initWithString:@"name"]];
                [hm setObject:[[DLString alloc] initWithString:[sym fnName]] forKey:[[DLKeyword alloc] initWithString:@"function-name"]];
                [hm setObject:[[DLString alloc] initWithString:[sym initialModuleName] ? [sym initialModuleName] : @"*"] forKey:[[DLKeyword alloc] initWithString:@"initial-module"]];
                [hm setObject:[[DLNumber alloc] initWithInteger:[sym arity]] forKey:[[DLKeyword alloc] initWithString:@"arity"]];
                [hm setObject:[[DLNumber alloc] initWithInteger:[sym initialArity]] forKey:[[DLKeyword alloc] initWithString:@"initial-arity"]];
                [hm setObject:[[DLBool alloc] initWithBool:[sym isFault]] forKey:[[DLKeyword alloc] initWithString:@"fault?"]];
                [hm setObject:[[DLBool alloc] initWithBool:[sym isFunction]] forKey:[[DLKeyword alloc] initWithString:@"function?"]];
                [hm setObject:[[DLBool alloc] initWithBool:[sym isModule]] forKey:[[DLKeyword alloc] initWithString:@"module?"]];
                [hm setObject:[[DLBool alloc] initWithBool:[sym isImported]] forKey:[[DLKeyword alloc] initWithString:@"imported?"]];
                [hm setObject:[[DLBool alloc] initWithBool:[sym isQualified]] forKey:[[DLKeyword alloc] initWithString:@"qualified?"]];
                [hm setObject:[[DLString alloc] initWithString:[sym string]] forKey:[[DLKeyword alloc] initWithString:@"value"]];
            } else if ([DLFunction isFunction:elem]) {
                DLFunction *fn = (DLFunction *)elem;
                [hm setObject:[[DLVector alloc] initWithArray:[fn params]] forKey:[[DLKeyword alloc] initWithString:@"arguments"]];
                [hm setObject:[[DLBool alloc] initWithBool:[fn isMacro]] forKey:[[DLKeyword alloc] initWithString:@"macro?"]];
                [hm setObject:[[DLNumber alloc] initWithInteger:[fn argsCount]] forKey:[[DLKeyword alloc] initWithString:@"arity"]];
                [hm setObject:[[DLString alloc] initWithString:[fn name]] forKey:[[DLKeyword alloc] initWithString:@"name"]];
                [hm setObject:[[DLBool alloc] initWithBool:[fn isImported]] forKey:[[DLKeyword alloc] initWithString:@"imported?"]];
            } else if ([DLList isList:elem]) {
                DLList *xs = (DLList *)elem;
                [hm setObject:[[DLNumber alloc] initWithInteger:[xs count]] forKey:[[DLKeyword alloc] initWithString:@"count"]];
                [hm setObject:[[DLBool alloc] initWithBool:[xs isEmpty]] forKey:[[DLKeyword alloc] initWithString:@"empty?"]];
                [hm setObject:[[DLBool alloc] initWithBool:[elem isMutable]] forKey:[[DLKeyword alloc] initWithString:@"mutable?"]];
            } else if ([DLVector isVector:elem]) {
                DLVector *vec = (DLVector *)elem;
                [hm setObject:[[DLNumber alloc] initWithInteger:[vec count]] forKey:[[DLKeyword alloc] initWithString:@"count"]];
                [hm setObject:[[DLBool alloc] initWithBool:[vec isEmpty]] forKey:[[DLKeyword alloc] initWithString:@"empty?"]];
                [hm setObject:[[DLBool alloc] initWithBool:[elem isMutable]] forKey:[[DLKeyword alloc] initWithString:@"mutable?"]];
            } else if ([DLHashMap isHashMap:elem]) {
                DLHashMap *hm = (DLHashMap *)elem;
                [hm setObject:[[DLNumber alloc] initWithInteger:[hm count]] forKey:[[DLKeyword alloc] initWithString:@"count"]];
                [hm setObject:[[DLBool alloc] initWithBool:[elem isMutable]] forKey:[[DLKeyword alloc] initWithString:@"mutable?"]];
            } else if ([DLKeyword isKeyword:elem]) {
                DLKeyword *kwd = (DLKeyword *)elem;
                [hm setObject:[kwd value] forKey:[[DLKeyword alloc] initWithString:@"name"]];
            } else if ([DLAtom isAtom:elem]) {
                DLAtom *atom = (DLAtom *)elem;
                [hm setObject:[[DLString alloc] initWithString:[[atom value] dataTypeName]] forKey:[[DLKeyword alloc] initWithString:@"value-type"]];
            } else if ([DLNumber isNumber:elem]) {
                DLNumber *num = (DLNumber *)elem;
                [hm setObject:[[DLBool alloc] initWithBool:[num isDouble]] forKey:[[DLKeyword alloc] initWithString:@"double?"]];
            } else if ([DLString isString:elem]) {
                DLString *str = (DLString *)elem;
                [hm setObject:[[DLBool alloc] initWithBool:[elem isMutable]] forKey:[[DLKeyword alloc] initWithString:@"mutable?"]];
                [hm setObject:[str value] forKey:[[DLKeyword alloc] initWithString:@"value"]];
            }
            return [[DLHashMap alloc] initWithMapTable:hm];
        }
    };
    fn = [[DLFunction alloc] initWithFn:info argCount:1 name:@"info/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"info" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Module

- (NSMapTable * _Nullable)moduleInfo:(NSString *)moduleName {
    @autoreleasepool {
        DLEnv *env = [DLEnv envForModuleName:moduleName];
        if (!env) {
            [[[DLError alloc] initWithFormat:DLModuleNotFound, moduleName] throw];
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
        [info setObject:[[DLVector alloc] initWithArray:[exportedFns mutableCopy]] forKey:[[DLKeyword alloc] initWithString:[DLConst exports]]];
        [info setObject:[[DLVector alloc] initWithArray:[importedFns mutableCopy]] forKey:[[DLKeyword alloc] initWithString:[DLConst imports]]];
        [info setObject:[[DLVector alloc] initWithArray:[internalFns mutableCopy]] forKey:[[DLKeyword alloc] initWithString:[DLConst internal]]];
        [info setObject:[[DLString alloc] initWithString:moduleName] forKey:[[DLKeyword alloc] initWithString:[DLConst name]]];
        [info setObject:[[DLString alloc] initWithString:[env moduleDescription]] forKey:[[DLKeyword alloc] initWithString:[DLConst description]]];
        return info;
    }
}

- (void)addModuleFunctions {
    DLFunction *fn = nil;

    #pragma mark current-module-name
    id<DLDataProtocol>(^currentModuleName)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            return [[DLString alloc] initWithString:[DLState currentModuleName]];
        }
    };
    fn = [[DLFunction alloc] initWithFn:currentModuleName argCount:0 name:@"current-module-name/0"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"current-module-name" moduleName:[DLConst coreModuleName]]];

    #pragma mark module-info
    id<DLDataProtocol>(^moduleInfo)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLString *mod = [DLString dataToString:[xs first] fnName:@"module-info/1"];
            NSString* moduleName = (NSString *)[mod value];
            NSMapTable *fns = [self moduleInfo:moduleName];
            if (!fns) return nil;
            return [[DLHashMap alloc] initWithMapTable:fns];
        }
    };
    fn = [[DLFunction alloc] initWithFn:moduleInfo argCount:1 name:@"module-info/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"module-info" moduleName:[DLConst coreModuleName]]];

    #pragma mark module-exist?
    id<DLDataProtocol>(^moduleExist)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            DLString *mod = [DLString dataToString:[xs first] fnName:@"module-exist?/1"];
            NSString* moduleName = (NSString *)[mod value];
            DLEnv *env = [DLEnv envForModuleName:moduleName];
            return [[DLBool alloc] initWithBool:(env != nil)];
        }
    };
    fn = [[DLFunction alloc] initWithFn:moduleExist argCount:1 name:@"module-exist?/1"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"module-exist?" moduleName:[DLConst coreModuleName]]];

    #pragma mark all-modules
    id<DLDataProtocol>(^allModules)(NSMutableArray *xs) = ^id<DLDataProtocol>(NSMutableArray *xs) {
        @autoreleasepool {
            NSArray *modArr = [[[DLEnv modules] allKeys] sortedArrayUsingFunction:sortAscending context:nil hint:[self allModulesSortHint]];
            [self setAllModulesSortHint:[modArr sortedArrayHint]];
            NSMutableArray *modules = [NSMutableArray new];
            NSString *name = nil;
            for (name in modArr) {
                [modules addObject:[[DLString alloc] initWithString:name]];
            }
            return [[DLVector alloc] initWithArray:modules];
        }
    };
    fn = [[DLFunction alloc] initWithFn:allModules argCount:0 name:@"all-modules/0"];
    [_env setObject:fn forKey:[[DLSymbol alloc] initWithFunction:fn name:@"all-modules" moduleName:[DLConst coreModuleName]]];
}

#pragma mark - Internal

- (DLEnv *)env {
    return _env;
}

- (NSData * _Nullable)allModulesSortHint {
    return _allModuleSortHint;
}

- (void)setAllModulesSortHint:(NSData *)hint {
    _allModuleSortHint = hint;
}

@end


