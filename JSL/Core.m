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
            NSDecimalNumber *n = [(JSNumber *)args[0] val];
            if ([n respondsToSelector:sel]) {
                num = objc_msgSend(n, sel, [(JSNumber *)args[1] val]);
            }
        }
        if (len > 2) {
            for (i = 2; i < len; i++) {
                num = objc_msgSend(num, sel, [(JSNumber *)args[i] val]);
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
        return [[JSNumber alloc] initWithInt:[(JSNumber *)args[0] intValue] % [(JSNumber *)args[1] intValue]];
    };
    [ns setObject:mod forKey:@"mod"];
}

- (void)addComparisonFunctions {
    JSData *(^lessThan)(JSNumber *lhs, JSNumber *rhs) = ^JSData *(JSNumber *lhs, JSNumber *rhs) {
        return [[JSBool alloc] initWithBool:[[lhs val] isLessThan:[rhs val]]];
    };
    [ns setObject:lessThan forKey:@"<"];
    JSData *(^greaterThan)(JSNumber *lhs, JSNumber *rhs) = ^JSData *(JSNumber *lhs, JSNumber *rhs) {
        return [[JSBool alloc] initWithBool:[[lhs val] isGreaterThan:[rhs val]]];
    };
    [ns setObject:greaterThan forKey:@">"];
    JSData *(^lessThanOrEqualTo)(JSNumber *lhs, JSNumber *rhs) = ^JSData *(JSNumber *lhs, JSNumber *rhs) {
        return [[JSBool alloc] initWithBool:[[lhs val] isLessThanOrEqualTo:[rhs val]]];
    };
    [ns setObject:lessThanOrEqualTo forKey:@"<="];
    JSData *(^greaterThanOrEqualTo)(JSNumber *lhs, JSNumber *rhs) = ^JSData *(JSNumber *lhs, JSNumber *rhs) {
        return [[JSBool alloc] initWithBool:[[lhs val] isGreaterThanOrEqualTo:[rhs val]]];
    };
    [ns setObject:greaterThanOrEqualTo forKey:@">="];
    JSData *(^equalTo)(JSNumber *lhs, JSNumber *rhs) = ^JSData *(JSNumber *lhs, JSNumber *rhs) {
        return [[JSBool alloc] initWithBool:[[lhs val] isEqualTo:[rhs val]]];
    };
    [ns setObject:equalTo forKey:@"="];
}

- (void)addPrintFunctions {
    Core * __weak weakSelf = self;
    JSData *(^println)(JSList *xs) = ^JSData *(JSList *xs) {
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
    [ns setObject:println forKey:@"println"];
    JSData *(^prn)(JSList *xs) = ^JSData *(JSList *xs) {
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
    [ns setObject:prn forKey:@"prn"];
    JSData *(^prstr)(JSList *xs) = ^JSData *(JSList *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->printer printStringFor:[xs nth:i] readably:true]];
        }
        return [[JSString alloc] initWithString:[ret componentsJoinedByString:@" "]];
    };
    [ns setObject:prstr forKey:@"pr-str"];
    JSData *(^str)(JSList *xs) = ^JSData *(JSList *xs) {
        Core *this = weakSelf;
        NSUInteger len = [xs count];
        NSUInteger i = 0;
        NSMutableArray *ret = [NSMutableArray new];
        for (i = 0; i < len; i++) {
            [ret addObject:[this->printer printStringFor:[xs nth:i] readably:false]];
        }
        return [[JSString alloc] initWithString:[ret componentsJoinedByString:@""]];
    };
    [ns setObject:str forKey:@"str"];
}

- (NSMutableDictionary *)namespace {
    return ns;
}

@end
