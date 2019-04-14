//
//  Core.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Core.h"

@implementation Core {
    NSMutableDictionary *ns;
    Reader *reader;
    Printer *printer;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    ns = [NSMutableDictionary new];
    reader = [Reader new];
    printer = [Printer new];
    [self addArithmeticFunctions];
    [self addComparisonFunctions];
    [self addPrintFunctions];
    [self addListFunctions];
    [self addEvalFunctions];
    [self addAtomFunctions];
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
        return calc(args, @selector(decimalNumberByAdding:));
    }];
    [ns setObject:add forKey:@"+"];

    JSFunction *subtract = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return calc(args, @selector(decimalNumberBySubtracting:));
    }];
    [ns setObject:subtract forKey:@"-"];

    JSFunction *multiply = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return calc(args, @selector(decimalNumberByMultiplyingBy:));
    }];
    [ns setObject:multiply forKey:@"*"];

    JSFunction *divide = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return calc(args, @selector(decimalNumberByDividingBy:));
    }];
    [ns setObject:divide forKey:@"/"];

    JSData *(^mod)(NSMutableArray *args) = ^JSData *(NSMutableArray *args) {
        if ([args count] != 2) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
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
    [ns setObject:[[JSFunction alloc] initWithFn:mod] forKey:@"mod"];
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
        if ([args count] != 2) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        return [[JSBool alloc] initWithBool:(BOOL)objc_msgSend([(JSNumber *)[args first] value], sel, [(JSNumber *)[args second] value])];
    };

    JSFunction *lessThan = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isLessThan:));
    }];
    [ns setObject:lessThan forKey:@"<"];

    JSFunction *greaterThan = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isGreaterThan:));
    }];
    [ns setObject:greaterThan forKey:@">"];

    JSFunction *lessThanOrEqualTo = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isLessThanOrEqualTo:));
    }];
    [ns setObject:lessThanOrEqualTo forKey:@"<="];

    JSFunction *greaterThanOrEqualTo = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        return compare(args, @selector(isGreaterThanOrEqualTo:));
    }];
    [ns setObject:greaterThanOrEqualTo forKey:@">="];

    JSFunction *equalTo = [[JSFunction alloc] initWithFn:^JSData *(NSMutableArray * args) {
        if ([args count] != 2) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        return [[JSBool alloc] initWithBool:[self isEqual:(JSData *)[args first] rhs:(JSData *)[args second]]];
    }];
    [ns setObject:equalTo forKey:@"="];
}

- (void)addPrintFunctions {
    Core * __weak weakSelf = self;
    JSData *(^println)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->printer printStringFor:(JSData *)[xs nth:i] readably:false]];
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:println] forKey:@"println"];

    JSData *(^prn)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->printer printStringFor:[xs nth:i] readably:true]];
        }
        info(@"%@", [ret componentsJoinedByString:@" "]);
        return [JSNil new];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:prn] forKey:@"prn"];

    JSData *(^prstr)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->printer printStringFor:[xs nth:i] readably:true]];
        }
        return [[JSString alloc] initWithString:[ret componentsJoinedByString:@" "]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:prstr] forKey:@"pr-str"];

    JSData *(^str)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->printer printStringFor:[xs nth:i] readably:false]];
        }
        return [[JSString alloc] initWithString:[ret componentsJoinedByString:@""]];
    };
    [ns setObject:[[JSFunction alloc]initWithFn:str] forKey:@"str"];
}

- (void)addListFunctions {
    JSData *(^list)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSList alloc] initWithArray:xs];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:list] forKey:@"list"];

    JSData *(^listp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[[(JSData *)[xs first] dataType] isEqual:@"JSList"]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:listp] forKey:@"list?"];

    JSData *(^emptyp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        return [[JSBool alloc] initWithBool:[(JSList *)[xs first] isEmpty]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:emptyp] forKey:@"empty?"];

    JSData *(^count)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        if ([[(JSData *)[xs first] dataType] isEqual:@"JSNil"]) {
            return [[JSNumber alloc] initWithInteger:0];
        }
        return [[JSNumber alloc] initWithInteger:[(JSList *)[xs first] count]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:count] forKey:@"count"];

    JSData *(^cons)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *data = (JSData *)[xs second];
        NSMutableArray *arr = [(JSList *)data value];
        [arr insertObject:(JSData *)[xs first] atIndex:0];
        if ([[data dataType] isEqual:@"JSVector"]) {
            return [[JSVector alloc] initWithArray:arr];
        }
        return [[JSList alloc] initWithArray:arr];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:cons] forKey:@"cons"];

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
    [ns setObject:[[JSFunction alloc] initWithFn:concat] forKey:@"concat"];

    JSData *(^nth)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
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
    [ns setObject:[[JSFunction alloc] initWithFn:nth] forKey:@"nth"];

    JSData *(^first)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        NSMutableArray *list = [xs first];
        if ([list isEmpty]) {
            return [JSNil new];
        }
        return (JSList *)[list first];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:first] forKey:@"first"];

    JSData *(^rest)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        NSMutableArray *list = [xs first];
        return (JSList *)[list rest];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:rest] forKey:@"rest"];

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
    [ns setObject:[[JSFunction alloc] initWithFn:map] forKey:@"map"];

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
    [ns setObject:[[JSFunction alloc] initWithFn:conj] forKey:@"conj"];
}

- (void)addEvalFunctions {
    Core * __weak weakSelf = self;
    JSData *(^readString)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        Core *this = weakSelf;
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSString"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        return [this->reader readString:[(JSString *)first value]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:readString] forKey:@"read-string"];

    JSData *(^slurp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSString"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        return [[JSString alloc] initWithContentsOfFile:[(JSString *)first value]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:slurp] forKey:@"slurp"];
}

- (void)addAtomFunctions {
    JSData *(^atom)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        return [[JSAtom alloc] initWithData:first];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:atom] forKey:@"atom"];

    JSData *(^atomp)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        return [[JSBool alloc] initWithBool:[[first dataType] isEqual:@"JSAtom"]];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:atomp] forKey:@"atom?"];

    JSData *(^deref)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSAtom"]) {
            return [JSNil new];
        }
        return [(JSAtom *)first value];
    };
    [ns setObject:[[JSFunction alloc] initWithFn:deref] forKey:@"deref"];

    JSData *(^reset)(NSMutableArray *xs) = ^JSData *(NSMutableArray *xs) {
        JSData *first = (JSData *)[xs first];
        if ([[first dataType] isNotEqualTo:@"JSAtom"]) {
            @throw [[NSException alloc] initWithName:JSL_INVALID_ARGUMENT reason:JSL_INVALID_ARGUMENT_MSG userInfo:nil];
        }
        JSData *value = (JSData *)[xs second];
        [(JSAtom *)first setValue:value];
        return value;
    };
    [ns setObject:[[JSFunction alloc] initWithFn:reset] forKey:@"reset!"];

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
    [ns setObject:[[JSFunction alloc] initWithFn:swap] forKey:@"swap!"];
}

- (NSMutableDictionary *)namespace {
    return ns;
}

@end
