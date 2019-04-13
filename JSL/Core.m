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
            ret = [[JSNumber alloc] initWithInt:[lhs intValue] % [rhs intValue]];
        }
        return ret;
    };
    [ns setObject:[[JSFunction alloc] initWithFn:mod] forKey:@"mod"];
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
        return compare(args, @selector(isEqualTo:));
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

- (NSMutableDictionary *)namespace {
    return ns;
}

@end
