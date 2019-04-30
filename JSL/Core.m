//
//  Core.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Core.h"

@implementation Core {
    NSMutableDictionary *_ns;
    Reader *_reader;
    Printer *_printer;
    Terminal *_terminal;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _ns = [NSMutableDictionary new];
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

double dmod(double a, double n) {
    return a - n * floor(a / n);
}

- (void)addArithmeticFunctions {
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

    JSFunction *add = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        if ([args isEmpty]) return [[JSNumber alloc] initWithInt:0];
        return calc(args, @selector(decimalNumberByAdding:));
    }];
    [_ns setObject:add forKey:@"+"];

    JSFunction *subtract = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
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
    }];
    [_ns setObject:subtract forKey:@"-"];

    JSFunction *multiply = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        if ([args isEmpty]) return [[JSNumber alloc] initWithInt:1];
        return calc(args, @selector(decimalNumberByMultiplyingBy:));
    }];
    [_ns setObject:multiply forKey:@"*"];

    JSFunction *divide = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        if ([args count] == 1) [args insertObject:[[JSNumber alloc] initWithInteger:1] atIndex:0];
        return calc(args, @selector(decimalNumberByDividingBy:));
    }];
    [_ns setObject:divide forKey:@"/"];

    id<JSDataProtocol>(^mod)(NSMutableArray *args) = ^id<JSDataProtocol>(NSMutableArray *args) {
        [TypeUtils checkArity:args arity:2];
        JSNumber *lhs = [JSNumber dataToNumber:[args first]];
        JSNumber *rhs = [JSNumber dataToNumber:[args second]];
        JSNumber *ret = nil;
        if ([lhs isDouble] || [rhs isDouble]) {
            ret = [[JSNumber alloc] initWithDouble:dmod([lhs doubleValue], [rhs doubleValue])];
        } else {
            ret = [[JSNumber alloc] initWithInteger:[lhs integerValue] % [rhs integerValue]];
        }
        return ret;
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:mod] forKey:@"mod"];
}

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

    JSFunction *lessThan = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        return compare(args, @selector(isLessThan:));
    }];
    [_ns setObject:lessThan forKey:@"<"];

    JSFunction *greaterThan = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        return compare(args, @selector(isGreaterThan:));
    }];
    [_ns setObject:greaterThan forKey:@">"];

    JSFunction *lessThanOrEqualTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        return compare(args, @selector(isLessThanOrEqualTo:));
    }];
    [_ns setObject:lessThanOrEqualTo forKey:@"<="];

    JSFunction *greaterThanOrEqualTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
        return compare(args, @selector(isGreaterThanOrEqualTo:));
    }];
    [_ns setObject:greaterThanOrEqualTo forKey:@">="];

    JSFunction *equalTo = [[JSFunction alloc] initWithFn:^id<JSDataProtocol>(NSMutableArray * args) {
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
    }];
    [_ns setObject:equalTo forKey:@"="];
}

- (void)addPrintFunctions {
    Core * __weak weakSelf = self;
    id<JSDataProtocol>(^println)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:false]];
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:println] forKey:@"println"];

    id<JSDataProtocol>(^prn)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:[xs nth:i] readably:true]];
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:prn] forKey:@"prn"];

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
    [_ns setObject:[[JSFunction alloc] initWithFn:prstr] forKey:@"pr-str"];

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
    [_ns setObject:[[JSFunction alloc]initWithFn:str] forKey:@"str"];
}

- (void)addListFunctions {
    id<JSDataProtocol>(^list)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSList alloc] initWithArray:xs];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:list] forKey:@"list"];

    id<JSDataProtocol>(^listp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        return [[JSBool alloc] initWithBool:[[(id<JSDataProtocol>)[xs first] dataType] isEqual:@"JSList"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:listp] forKey:@"list?"];

    id<JSDataProtocol>(^emptyp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        BOOL ret = YES;
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        ret = [JSString isString:first] ? [(JSString *)first isEmpty] : [[JSList dataToList:first] isEmpty];
        return [[JSBool alloc] initWithBool:ret];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:emptyp] forKey:@"empty?"];

    id<JSDataProtocol>(^count)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSNil isNil:first]) return [[JSNumber alloc] initWithInteger:0];
        return [[JSNumber alloc] initWithInteger:[JSString isString:first] ? [(JSString *)first count] : [[JSList dataToList:first] count]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:count] forKey:@"count"];

    id<JSDataProtocol>(^cons)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs second];
        NSMutableArray *arr = [[[JSList dataToList:data] value] mutableCopy];
        [arr insertObject:(id<JSDataProtocol>)[xs first] atIndex:0];
        return [[JSList alloc] initWithArray:arr];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:cons] forKey:@"cons"];

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
            list = [JSVector dataToList:data];
            jlen = [list count];
            for (j = 0; j < jlen; j++) {
                [arr addObject:[list nth:j]];
            }
        }
        return [[JSList alloc] initWithArray:arr];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:concat] forKey:@"concat"];

    id<JSDataProtocol>(^nth)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:2];
        id<JSDataProtocol> first = [xs first];
        id<JSDataProtocol> second = [xs second];
        NSMutableArray *list = [[JSVector dataToList:first position:1] value];
        JSNumber *num = [JSNumber dataToNumber:second position:2];
        NSUInteger n = [num integerValue];
        [TypeUtils checkIndexBounds:list index:n];
        return [list nth:n];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:nth] forKey:@"nth"];

    id<JSDataProtocol>(^first)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        NSMutableArray *list = [xs first];
        if ([xs isEmpty] || [JSNil isNil:list]) return [JSNil new];
        id<JSDataProtocol> first = (id<JSDataProtocol>)[list first];
        return (first == nil) ? [JSNil new] : first;
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:first] forKey:@"first"];

    id<JSDataProtocol>(^rest)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        [TypeUtils checkArity:xs arity:1];
        NSMutableArray *list = [xs first];
        return ([[(id<JSDataProtocol>)list dataType] isEqual:@"JSNil"] || [list isEmpty]) ? [JSList new] : (JSList *)[list rest];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:rest] forKey:@"rest"];

    id<JSDataProtocol>(^map)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        JSFunction *fn = [JSFunction dataToFunction:first];
        NSMutableArray *list = [[JSList dataToList:[xs second]] value];
        NSUInteger i = 0;
        NSUInteger len = [list count];
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[fn apply:[@[[list nth:i]] mutableCopy]]];
        }
        return [[JSList alloc] initWithArray:ret];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:map] forKey:@"map"];

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
    [_ns setObject:[[JSFunction alloc] initWithFn:conj] forKey:@"conj"];

    id<JSDataProtocol>(^sequentialp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSList isList:first] || [JSVector isVector:first])];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:sequentialp] forKey:@"sequential?"];

    id<JSDataProtocol>(^seq)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
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
        @throw [[NSException alloc] initWithName:JSL_NOT_A_SEQUENCE_ERROR reason:JSL_NOT_A_SEQUENCE_ERROR_MSG userInfo:nil];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:seq] forKey:@"seq"];
}

- (void)addEvalFunctions {
    Core * __weak weakSelf = self;
    id<JSDataProtocol>(^readString)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        return [this->_reader readString:[[JSString dataToString:[xs first]] value]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:readString] forKey:@"read-string"];

    id<JSDataProtocol>(^slurp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSString alloc] initWithContentsOfFile:[[JSString dataToString:[xs first]] value]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:slurp] forKey:@"slurp"];
}

- (void)addAtomFunctions {
    id<JSDataProtocol>(^atom)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSAtom alloc] initWithData:[xs first]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:atom] forKey:@"atom"];

    id<JSDataProtocol>(^atomp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSAtom isAtom:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:atomp] forKey:@"atom?"];

    id<JSDataProtocol>(^deref)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return ![JSAtom isAtom:first] ? [JSNil new] : [(JSAtom *)first value];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:deref] forKey:@"deref"];

    id<JSDataProtocol>(^reset)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        JSAtom *atom = [JSAtom dataToAtom:first];
        id<JSDataProtocol>value = (id<JSDataProtocol>)[xs second];
        [atom setValue:value];
        return value;
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:reset] forKey:@"reset!"];

    id<JSDataProtocol>(^swap)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        id<JSDataProtocol> second = (id<JSDataProtocol>)[xs second];
        JSAtom *atom = [JSAtom dataToAtom:first];
        JSFunction *fn = [JSFunction dataToFunction:second];
        NSMutableArray *more = [xs drop:2];
        [more insertObject:[atom value] atIndex:0];
        [atom setValue:[fn fn](more)];
        return [atom value];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:swap] forKey:@"swap!"];
}

- (void)addInvokeFunctions {
    id<JSDataProtocol>(^throw)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        @throw [[NSException alloc] initWithName:JSLException reason:JSLException userInfo:@{@"jsdata": (id<JSDataProtocol>)[xs first]}];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:throw] forKey:@"throw"];

    id<JSDataProtocol>(^apply)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        if ([[(id<JSDataProtocol>)[xs first] dataType] isEqual:@"JSFunction"]) {
            JSFunction *fn = (JSFunction *)[xs first];
            NSMutableArray *last = [[(JSList *)[xs last] value] mutableCopy];
            NSMutableArray *params = [NSMutableArray new];
            NSMutableArray *args = [xs mutableCopy];
            if ([args count] >= 2) {
                args = [args drop:1];
                args = [args dropLast];
            }
            if (args) params = args;
            [params addObjectsFromArray:last];
            return [fn apply:params];
        }
        @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:apply] forKey:@"apply"];
}

- (void)addPredicateFunctions {
    id<JSDataProtocol>(^nilp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSNil isNil:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:nilp] forKey:@"nil?"];

    id<JSDataProtocol>(^truep)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSBool isBool:data] && [(JSBool *)data value] == YES)];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:truep] forKey:@"true?"];

    id<JSDataProtocol>(^falsep)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSBool isBool:data] && [(JSBool *)data value] == NO)];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:falsep] forKey:@"false?"];

    id<JSDataProtocol>(^stringp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSString isString:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:stringp] forKey:@"string?"];

    id<JSDataProtocol>(^numberp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSNumber isNumber:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:numberp] forKey:@"number?"];

    id<JSDataProtocol>(^fnp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSFunction isFunction:first]) {
            JSFunction *fn = (JSFunction *)first;
            if (![fn isMacro]) return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:fnp] forKey:@"fn?"];

    id<JSDataProtocol>(^macrop)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSFunction isFunction:first] && [(JSFunction *)first isMacro])];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:macrop] forKey:@"macro?"];
}

- (void)addSymbolFunctions {
    id<JSDataProtocol>(^symbolp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSSymbol isSymbol:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:symbolp] forKey:@"symbol?"];

    id<JSDataProtocol>(^symbol)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSSymbol alloc] initWithName:[(JSString *)[xs first] value]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:symbol] forKey:@"symbol"];
}

- (void)addKeywordFunctions {
    id<JSDataProtocol>(^keyword)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol> )[xs first];
        if ([JSKeyword isKeyword:first]) return first;
        return ([JSString isString:first]) ? [[JSKeyword alloc] initWithString:[(JSString *)first value]] :[JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:keyword] forKey:@"keyword"];

    id<JSDataProtocol>(^keywordp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        return [[JSBool alloc] initWithBool:([JSKeyword isKeyword:first] || ([NSString isString:first] && [JSKeyword isEncodedKeyword:(NSString *)first]))];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:keywordp] forKey:@"keyword?"];
}

- (void)addVectorFunctions {
    id<JSDataProtocol>(^vector)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSVector alloc] initWithArray:xs];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:vector] forKey:@"vector"];

    id<JSDataProtocol>(^vectorp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSVector isVector:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:vectorp] forKey:@"vector?"];
}

- (void)addHashMapFunctions {
    id<JSDataProtocol>(^hashmap)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSHashMap alloc] initWithArray:xs];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:hashmap] forKey:@"hash-map"];

    id<JSDataProtocol>(^mapp)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[JSHashMap isHashMap:[xs first]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:mapp] forKey:@"map?"];

    id<JSDataProtocol>(^assoc)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first]];
        NSMapTable *table = [[first value] mutableCopy];
        NSMapTable *rest = [[[JSHashMap alloc] initWithArray:[xs rest]] value];
        return [[JSHashMap alloc] initWithMapTable:[table assoc:rest]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:assoc] forKey:@"assoc"];

    id<JSDataProtocol>(^dissoc)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first]];
        NSArray *keys = [xs rest];
        NSMapTable *table = [[first value] mutableCopy];
        return [[JSHashMap alloc] initWithMapTable:[table dissoc:keys]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:dissoc] forKey:@"dissoc"];

    id<JSDataProtocol>(^get)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> data = (id<JSDataProtocol>)[xs first];
        if ([JSHashMap isHashMap:data]) {
            JSHashMap *first = (JSHashMap *)data;
            id<JSDataProtocol> ret = (id<JSDataProtocol>)[first objectForKey:[xs second]];
            if (ret) return ret;
        }
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:get] forKey:@"get"];

    id<JSDataProtocol>(^contains)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first]];
        return [[JSBool alloc] initWithBool:[first containsKey:[xs second]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:contains] forKey:@"contains?"];

    id<JSDataProtocol>(^keys)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first]];
        return [[JSList alloc] initWithArray:[first allKeys]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:keys] forKey:@"keys"];

    id<JSDataProtocol>(^vals)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        JSHashMap *first = [JSHashMap dataToHashMap:[xs first]];
        return [[JSList alloc] initWithArray:[first allObjects]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:vals] forKey:@"vals"];
}

- (void)addIOFunctions {
    Core * __weak weakSelf = self;
    id<JSDataProtocol>(^readline)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        Core *this = weakSelf;
        return [[JSString alloc] initWithString:[this->_terminal readlineWithPrompt:[[[JSString dataToString:[xs first]] value] UTF8String]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:readline] forKey:@"readline"];
}

- (void)addMetaFunctions {
    id<JSDataProtocol>(^meta)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        id<JSDataProtocol> first = (id<JSDataProtocol>)[xs first];
        if ([JSFunction isFunction:first]) {
            JSFunction *fn = (JSFunction *)first;
            if ([fn meta]) return [fn meta];
        }
        return [first meta] ? [first meta] : [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:meta] forKey:@"meta"];

    id<JSDataProtocol>(^withMeta)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
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
    [_ns setObject:[[JSFunction alloc] initWithFn:withMeta] forKey:@"with-meta"];
}

- (void)addMiscFunctions {
    id<JSDataProtocol>(^exitfn)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        exit(0);
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:exitfn] forKey:@"exit*"];

    id<JSDataProtocol>(^timems)(NSMutableArray *xs) = ^id<JSDataProtocol>(NSMutableArray *xs) {
        return [[JSNumber alloc] initWithInteger:[Utils timestamp]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:timems] forKey:@"time-ms"];
}

- (NSMutableDictionary *)namespace {
    return _ns;
}

@end
