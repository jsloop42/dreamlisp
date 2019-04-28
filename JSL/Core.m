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
    JSData *(^calc)(NSMutableArray *args, SEL sel) = ^JSData *(NSMutableArray *args, SEL sel) {
        NSDecimalNumber *num = [NSDecimalNumber new];
        NSUInteger len = [args count];
        NSUInteger i = 0;
        if (len == 0) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        } else if (len == 1) {
            return (JSNumber *)args[0];
        }
        if (len >= 2) {
            NSDecimalNumber *n = [(JSNumber *)args[0] value];
            if ([n respondsToSelector:sel]) {
                num = objc_msgSend(n, sel, [(JSNumber *)args[1] value]);
            }
        }
        if (len > 2) {
            for (i = 2; i < len; i++) {
                num = objc_msgSend(num, sel, [(JSNumber *)args[i] value]);
            }
        }
        return [[JSNumber alloc] initWithNumber:num];
    };

    JSFunction *add = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        if ([args isEmpty]) { return [[JSNumber alloc] initWithInt:0]; }
        return calc(args, @selector(decimalNumberByAdding:));
    }];
    [_ns setObject:add forKey:@"+"];

    JSFunction *subtract = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return calc(args, @selector(decimalNumberBySubtracting:));
    }];
    [_ns setObject:subtract forKey:@"-"];

    JSFunction *multiply = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        if ([args isEmpty]) { return [[JSNumber alloc] initWithInt:1]; }
        return calc(args, @selector(decimalNumberByMultiplyingBy:));
    }];
    [_ns setObject:multiply forKey:@"*"];

    JSFunction *divide = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return calc(args, @selector(decimalNumberByDividingBy:));
    }];
    [_ns setObject:divide forKey:@"/"];

    JSData *(^mod)(NSMutableArray *args) = ^JSData *(NSMutableArray *args) {
        if ([args count] != 2) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 2, [args count]];
            [info throw];
        }
        JSNumber *lhs = (JSNumber *)[args first];
        JSNumber *rhs = (JSNumber *)[args second];
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

- (BOOL)isEqual:(JSData *)lhs rhs:(JSData *)rhs {
    if ([[lhs dataType] isEqual:@"JSNumber"] && [[rhs dataType] isEqual:@"JSNumber"]) {
        return [(JSNumber *)lhs isEqual:(JSNumber *)rhs];
    } else if ([[lhs dataType] isEqual:@"JSSymbol"] && [[rhs dataType] isEqual:@"JSSymbol"]) {
        return [(JSSymbol *)lhs isEqual:(JSSymbol *)rhs];
    } else if ([[lhs dataType] isEqual:@"JSString"] && [[rhs dataType] isEqual:@"JSString"]) {
        return [(JSString *)lhs isEqual:(JSString *)rhs];
    } else if ([[lhs dataType] isEqual:@"JSKeyword"] && [[rhs dataType] isEqual:@"JSKeyword"]) {
        return [(JSString *)lhs isEqual:(JSString *)rhs];
    } else if (([[lhs dataType] isEqual:@"JSList"] && [[rhs dataType] isEqual:@"JSList"]) ||
               ([[lhs dataType] isEqual:@"JSList"] && [[rhs dataType] isEqual:@"JSVector"]) ||
               ([[lhs dataType] isEqual:@"JSVector"] && [[rhs dataType] isEqual:@"JSList"]) ||
               ([[lhs dataType] isEqual:@"JSVector"] && [[rhs dataType] isEqual:@"JSVector"])) {
        return [(JSList *)lhs isEqual:(JSList *)rhs];
    } else if ([[lhs dataType] isEqual:@"JSHashMap"] && [[rhs dataType] isEqual:@"JSHashMap"]) {
        return [(JSHashMap *)lhs isEqual:(JSHashMap *)rhs];
    } else if ([[lhs dataType] isEqual:@"JSNil"] && [[rhs dataType] isEqual:@"JSNil"]) {
        return YES;
    } else if ([[lhs dataType] isEqual:@"JSBool"] && [[rhs dataType] isEqual:@"JSBool"]) {
        return [(JSBool *)lhs isEqual:(JSBool *)rhs];
    }
    return NO;
}

- (void)addComparisonFunctions {
    JSData *(^compare)(NSMutableArray *args, SEL sel) = ^JSData *(NSMutableArray *args, SEL sel) {
        BOOL ret = YES;
        NSUInteger i = 0;
        NSUInteger len = [args count];
        if (len >= 2) {
            for (i = 0; i < len - 1; i++) {
                ret = (BOOL)objc_msgSend([(JSNumber *)args[i] value], sel, [(JSNumber *)args[i + 1] value]);
                if (!ret) { break; }
            }
        } else if (len == 0) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, len];
            [info throw];
        }
        return [[JSBool alloc] initWithBool:ret];
    };

    JSFunction *lessThan = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isLessThan:));
    }];
    [_ns setObject:lessThan forKey:@"<"];

    JSFunction *greaterThan = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isGreaterThan:));
    }];
    [_ns setObject:greaterThan forKey:@">"];

    JSFunction *lessThanOrEqualTo = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isLessThanOrEqualTo:));
    }];
    [_ns setObject:lessThanOrEqualTo forKey:@"<="];

    JSFunction *greaterThanOrEqualTo = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isGreaterThanOrEqualTo:));
    }];
    [_ns setObject:greaterThanOrEqualTo forKey:@">="];

    JSFunction *equalTo = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        BOOL ret = YES;
        NSUInteger len = [args count];
        NSUInteger i = 0;
        if (len >= 2) {
            for (i = 0; i < len - 1; i++) {
                ret = [self isEqual:(JSData *)args[i] rhs:(JSData *)args[i + 1]];
                if (!ret) { break; }
            }
        } else if (len == 0) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, len];
            [info throw];
        }
        return [[JSBool alloc] initWithBool:ret];
    }];
    [_ns setObject:equalTo forKey:@"="];
}

- (void)addPrintFunctions {
    Core * __weak weakSelf = self;
    JSData *(^println)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->_printer printStringFor:(JSData *)[xs nth:i] readably:false]];
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:println] forKey:@"println"];

    JSData *(^prn)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
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

    JSData *(^prstr)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
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

    JSData *(^str)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
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
    JSData *(^list)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSList alloc] initWithArray:xs];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:list] forKey:@"list"];

    JSData *(^listp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] != 1) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, [xs count]];
            [info throw];
        }
        return [[JSBool alloc] initWithBool:[[(JSData *)[xs first] dataType] isEqual:@"JSList"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:listp] forKey:@"list?"];

    JSData *(^emptyp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] != 1) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, [xs count]];
            [info throw];
        }
        return [[JSBool alloc] initWithBool:[(JSList *)[xs first] isEmpty]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:emptyp] forKey:@"empty?"];

    JSData *(^count)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] != 1) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, [xs count]];
            [info throw];
        }
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSNil"]) {
            return [[JSNumber alloc] initWithInteger:0];
        }
        return [[JSNumber alloc] initWithInteger:[(JSList *)[xs first] count]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:count] forKey:@"count"];

    JSData *(^cons)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *data = (JSData *)[xs second];
        NSMutableArray *arr = [[(JSList *)data value] mutableCopy];
        [arr insertObject:(JSData *)[xs first] atIndex:0];
        return [[JSList alloc] initWithArray:arr];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:cons] forKey:@"cons"];

    JSData *(^concat)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        NSMutableArray *arr = [NSMutableArray new];
        NSUInteger i = 0;
        NSUInteger j = 0;
        NSUInteger len = [xs count];
        NSUInteger jlen = 0;
        JSList *list = nil;
        for (i = 0; i < len; i++) {
            list = (JSList *)[xs nth:i];
            jlen = [list count];
            for (j = 0; j < jlen; j++) {
                [arr addObject:[list nth:j]];
            }
        }
        return [[JSList alloc] initWithArray:arr];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:concat] forKey:@"concat"];

    JSData *(^nth)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] != 2) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 2, [xs count]];
            [info throw];
        }
        JSData *second = [xs second];
        if ([xs first] == nil || second == nil || [[second dataType] isNotEqualTo:@"JSNumber"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        NSMutableArray *list = [(JSList *)[xs first] value];
        JSNumber *num = (JSNumber *)second;
        NSUInteger n = [num integerValue];
        if (n >= [list count]) {
            @throw [[NSException alloc] initWithName:JSL_INDEX_OUT_OF_BOUNDS reason:JSL_INDEX_OUT_OF_BOUNDS_MSG userInfo:nil];
        }
        return [list nth:n];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:nth] forKey:@"nth"];

    JSData *(^first)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] != 1) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, [xs count]];
            [info throw];
        }
        NSMutableArray *list = [xs first];
        if ([xs isEmpty] || [[(JSData *)list dataType] isEqual:@"JSNil"]) {
            return [JSNil new];
        }
        JSData *first = (JSData *)[list first];
        if (first == nil) {
            return [JSNil new];
        }
        return first;
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:first] forKey:@"first"];

    JSData *(^rest)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] != 1) {
            JSError *info = [[JSError alloc] initWithFormat:ArityError, 1, [xs count]];
            [info throw];
        }
        NSMutableArray *list = [xs first];
        if ([[(JSData *)list dataType] isEqual:@"JSNil"] || [list isEmpty]) {
            return [JSList new];
        }
        return (JSList *)[list rest];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:rest] forKey:@"rest"];

    JSData *(^map)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if (!first || [[first dataType] isNotEqualTo:@"JSFunction"]) {
            @throw [[NSException alloc] initWithName:JSL_INDEX_OUT_OF_BOUNDS reason:JSL_INDEX_OUT_OF_BOUNDS_MSG userInfo:nil];
        }
        JSFunction *fn = (JSFunction *)first;
        NSMutableArray *list = [(JSList *)[xs second] value];
        NSUInteger i = 0;
        NSUInteger len = [list count];
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[fn apply:[@[[list nth:i]] mutableCopy]]];
        }
        return [[JSList alloc] initWithArray:ret];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:map] forKey:@"map"];

    JSData *(^conj)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        NSMutableArray *list = nil;
        NSMutableArray *rest = [xs rest];
        if ([[first dataType] isEqual:@"JSVector"]) {
            list = [(JSVector *)first value];
            [list addObjectsFromArray:rest];
            return [[JSVector alloc] initWithArray:list];
        } else if ([[first dataType] isEqual:@"JSList"]) {
            list = [(JSList *)first value];
        }
        return [[JSList alloc] initWithArray:[[rest reverse] arrayByAddingObjectsFromArray:list]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:conj] forKey:@"conj"];

    JSData *(^sequentialp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isEqual:@"JSList"] || [[first dataType] isEqual:@"JSVector"]) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:sequentialp] forKey:@"sequential?"];

    JSData *(^seq)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([xs count] == 0) {
            return [JSNil new];
        }
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isEqual:@"JSList"]) {
            JSList *list = (JSList *)first;
            if ([list count] == 0) return [JSNil new];
            return list;
        }
        if ([[first dataType] isEqual:@"JSVector"]) {
            JSVector *vec = (JSVector *)first;
            if ([vec count] == 0) return [JSNil new];
            return [vec list];
        }
        if ([[first dataType] isEqual:@"JSString"]) {
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
        if ([[first dataType] isEqual:@"JSNil"]) {
            return (JSNil *)first;
        }
        @throw [[NSException alloc] initWithName:JSL_NOT_A_SEQUENCE_ERROR reason:JSL_NOT_A_SEQUENCE_ERROR_MSG userInfo:nil];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:seq] forKey:@"seq"];
}

- (void)addEvalFunctions {
    Core * __weak weakSelf = self;
    JSData *(^readString)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSString"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        return [this->_reader readString:[(JSString *)first value]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:readString] forKey:@"read-string"];

    JSData *(^slurp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSString"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        return [[JSString alloc] initWithContentsOfFile:[(JSString *)first value]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:slurp] forKey:@"slurp"];
}

- (void)addAtomFunctions {
    JSData *(^atom)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        return [[JSAtom alloc] initWithData:first];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:atom] forKey:@"atom"];

    JSData *(^atomp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        return [[JSBool alloc] initWithBool:[[first dataType] isEqual:@"JSAtom"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:atomp] forKey:@"atom?"];

    JSData *(^deref)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSAtom"]) {
            return [JSNil new];
        }
        return [(JSAtom *)first value];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:deref] forKey:@"deref"];

    JSData *(^reset)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSAtom"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        JSData *value = (JSData *)[xs second];
        [(JSAtom *)first setValue:value];
        return value;
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:reset] forKey:@"reset!"];

    JSData *(^swap)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        JSData *second = (JSData *)[xs second];
        if ([[first dataType] isNotEqualTo:@"JSAtom"] || [[second dataType] isNotEqualTo:@"JSFunction"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        JSAtom *atom = (JSAtom *)first;
        JSFunction *fn = (JSFunction *)second;
        NSMutableArray *more = [xs drop:2];
        [more insertObject:[atom value] atIndex:0];
        [atom setValue:[fn fn](more)];
        return [atom value];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:swap] forKey:@"swap!"];
}

- (void)addInvokeFunctions {
    JSData *(^throw)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        @throw [[NSException alloc] initWithName:JSLException reason:JSLException userInfo:@{@"jsdata": (JSData *)[xs first]}];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:throw] forKey:@"throw"];

    JSData *(^apply)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSFunction"]) {
            JSFunction *fn = (JSFunction *)[xs first];
            NSMutableArray *last = [[(JSList *)[xs last] value] mutableCopy];
            NSMutableArray *params = [NSMutableArray new];
            NSMutableArray *args = [xs mutableCopy];
            if ([args count] >= 2) {
                args = [args drop:1];
                args = [args dropLast];
            }
            if (args) {
                params = args;
            }
            [params addObjectsFromArray:last];
            return [fn apply:params];
        }
        @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:apply] forKey:@"apply"];
}

- (void)addPredicateFunctions {
    JSData *(^nilp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[[(JSData *)[xs first] dataType] isEqual:@"JSNil"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:nilp] forKey:@"nil?"];

    JSData *(^truep)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSBool"] && [(JSBool *)[xs first] value] == YES) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:truep] forKey:@"true?"];

    JSData *(^falsep)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSBool"] && [(JSBool *)[xs first] value] == NO) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:falsep] forKey:@"false?"];

    JSData *(^stringp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSString"]) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:stringp] forKey:@"string?"];

    JSData *(^numberp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSNumber"]) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:numberp] forKey:@"number?"];

    JSData *(^fnp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData * first = (JSData *)[xs first];
        if ([[first dataType] isEqual:@"JSFunction"]) {
            JSFunction *fn = (JSFunction *)first;
            if (![fn isMacro]) {
                return [[JSBool alloc] initWithBool:YES];
            }
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:fnp] forKey:@"fn?"];

    JSData *(^macrop)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData * first = (JSData *)[xs first];
        if ([[first dataType] isEqual:@"JSFunction"] && [(JSFunction *)first isMacro]) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:macrop] forKey:@"macro?"];
}

- (void)addSymbolFunctions {
    JSData *(^symbolp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[[(JSData *)[xs first] dataType] isEqual:@"JSSymbol"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:symbolp] forKey:@"symbol?"];

    JSData *(^symbol)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSSymbol alloc] initWithName:[(JSString *)[xs first] value]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:symbol] forKey:@"symbol"];
}

- (void)addKeywordFunctions {
    JSData *(^keyword)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isEqual:@"JSKeyword"]) {
            return first;
        } else if ([[first dataType] isEqual:@"JSString"]) {
            return [[JSKeyword alloc] initWithString:[(JSString *)first value]];
        }
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:keyword] forKey:@"keyword"];

    JSData *(^keywordp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSKeyword"]
            || ([NSStringFromClass([(JSData *)[xs first] classForCoder]) isEqual:@"NSString"]
                && [JSKeyword isKeyword:(NSString *)[xs first]])) {
            return [[JSBool alloc] initWithBool:YES];
        }
        return [[JSBool alloc] initWithBool:NO];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:keywordp] forKey:@"keyword?"];
}

- (void)addVectorFunctions {
    JSData *(^vector)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSVector alloc] initWithArray:xs];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:vector] forKey:@"vector"];

    JSData *(^vectorp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[[(JSData *)[xs first] dataType] isEqual:@"JSVector"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:vectorp] forKey:@"vector?"];
}

- (void)addHashMapFunctions {
    JSData *(^hashmap)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSHashMap alloc] initWithArray:xs];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:hashmap] forKey:@"hash-map"];

    JSData *(^mapp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[[(JSData *)[xs first] dataType] isEqual:@"JSHashMap"]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:mapp] forKey:@"map?"];

    JSData *(^assoc)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSHashMap *first = (JSHashMap *)[xs first];
        NSMapTable *table = [[first value] mutableCopy];
        NSMapTable *rest = [[[JSHashMap alloc] initWithArray:[xs rest]] value];
        return [[JSHashMap alloc] initWithMapTable:[table assoc:rest]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:assoc] forKey:@"assoc"];

    JSData *(^dissoc)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSHashMap *first = (JSHashMap *)[xs first];
        NSArray *keys = [xs rest];
        NSMapTable *table = [[first value] mutableCopy];
        return [[JSHashMap alloc] initWithMapTable:[table dissoc:keys]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:dissoc] forKey:@"dissoc"];

    JSData *(^get)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *data = (JSData *)[xs first];
        if ([[data dataType] isEqual:@"JSHashMap"]) {
            JSHashMap *first = (JSHashMap *)[xs first];
            JSData *ret = (JSData *)[first objectForKey:[xs second]];
            if (ret) { return ret; }
        }
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:get] forKey:@"get"];

    JSData *(^contains)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSHashMap *first = (JSHashMap *)[xs first];
        return [[JSBool alloc] initWithBool:[first containsKey:[xs second]]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:contains] forKey:@"contains?"];

    JSData *(^keys)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSHashMap *first = (JSHashMap *)[xs first];
        return [[JSList alloc] initWithArray:[first allKeys]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:keys] forKey:@"keys"];

    JSData *(^vals)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSHashMap *first = (JSHashMap *)[xs first];
        return [[JSList alloc] initWithArray:[first allObjects]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:vals] forKey:@"vals"];
}

- (void)addIOFunctions {
    Core * __weak weakSelf = self;
    JSData *(^readline)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSString *line = [this->_terminal readlineWithPrompt:[[(JSString *)[xs first] value] UTF8String]];
        return [[JSString alloc] initWithString:line];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:readline] forKey:@"readline"];
}

- (void)addMetaFunctions {
    JSData *(^meta)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isEqual:@"JSFunction"]) {
            JSFunction *fn = (JSFunction *)first;
            if ([fn meta]) {
                return [fn meta];
            }
        }
        if ([first meta]) return [first meta];
        return [JSNil new];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:meta] forKey:@"meta"];

    JSData *(^withMeta)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        JSData *meta = (JSData *)[xs second];
        NSString *dataType = [first dataType];
        if ([dataType isEqual:@"JSFunction"]) {
            JSFunction *fn = (JSFunction *)first;
            return [[JSFunction alloc] initWithMeta:meta func:fn];
        }
        if ([dataType isEqual:@"JSString"]) {
            return [[JSString alloc] initWithMeta:meta string:(JSString *)first];
        }
        if ([dataType isEqual:@"JSKeyword"]) {
            return [[JSKeyword alloc] initWithMeta:meta keyword:(JSKeyword *)first];
        }
        if ([dataType isEqual:@"JSSymbol"]) {
            return [[JSSymbol alloc] initWithMeta:meta symbol:(JSSymbol *)first];
        }
        if ([dataType isEqual:@"JSHashMap"]) {
            return [[JSHashMap alloc] initWithMeta:meta hashmap:(JSHashMap *)first];
        }
        if ([dataType isEqual:@"JSList"]) {
            return [[JSList alloc] initWithMeta:meta list:(JSList *)first];
        }
        if ([dataType isEqual:@"JSVector"]) {
            return [[JSVector alloc] initWithMeta:meta vector:(JSVector *)first];
        }
        if ([dataType isEqual:@"JSNumber"]) {
            return [[JSNumber alloc] initWithMeta:meta number:(JSNumber *)first];
        }
        if ([dataType isEqual:@"JSAtom"]) {
            return [[JSAtom alloc] initWithMeta:meta atom:(JSAtom *)first];
        }
        return first;
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:withMeta] forKey:@"with-meta"];
}

- (void)addMiscFunctions {
    JSData *(^exitfn)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        exit(0);
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:exitfn] forKey:@"exit*"];

    JSData *(^timems)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSNumber alloc] initWithInteger:[Utils timestamp]];
    };
    [_ns setObject:[[JSFunction alloc] initWithFn:timems] forKey:@"time-ms"];
}

- (NSMutableDictionary *)namespace {
    return _ns;
}

@end
