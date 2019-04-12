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
    JSData *(^add)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = ^JSData *(JSNumber *first, ...) {
        NSDecimalNumber *num = nil;
        va_list args;
        va_start(args, first);
        for (JSNumber *arg = first; arg != nil; arg = va_arg(args, JSNumber *)) {
            if (num != nil) {
                num = [num decimalNumberByAdding:[arg val]];
            } else {
                num = [arg val];
            }
        }
        va_end(args);
        return [[JSNumber alloc] initWithNumber:num];
    };
    [ns setObject:add forKey:@"+"];
    JSData *(^subtract)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = ^JSData *(JSNumber *first, ...) {
        NSDecimalNumber *num = nil;
        va_list args;
        va_start(args, first);
        for (JSNumber *arg = first; arg != nil; arg = va_arg(args, JSNumber *)) {
            if (num != nil) {
                num = [num decimalNumberBySubtracting:[arg val]];
            } else {
                num = [arg val];
            }
        }
        va_end(args);
        return [[JSNumber alloc] initWithNumber:num];
    };
    [ns setObject:subtract forKey:@"-"];
    JSData *(^multiply)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = ^JSData *(JSNumber *first, ...) {
        NSDecimalNumber *num = nil;
        va_list args;
        va_start(args, first);
        for (JSNumber *arg = first; arg != nil; arg = va_arg(args, JSNumber *)) {
            if (num != nil) {
                num = [num decimalNumberByMultiplyingBy:[arg val]];
            } else {
                num = [arg val];
            }
        }
        va_end(args);
        return [[JSNumber alloc] initWithNumber:num];
    };
    [ns setObject:multiply forKey:@"*"];
    JSData *(^divide)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = ^JSData *(JSNumber *first, ...) {
        NSDecimalNumber *num = nil;
        va_list args;
        va_start(args, first);
        for (JSNumber *arg = first; arg != nil; arg = va_arg(args, JSNumber *)) {
            if (num != nil) {
                num = [num decimalNumberByDividingBy:[arg val]];
            } else {
                num = [arg val];
            }
        }
        va_end(args);
        return [[JSNumber alloc] initWithNumber:num];
    };
    [ns setObject:divide forKey:@"/"];
    JSData *(^mod)(JSNumber *first, JSNumber *second) = ^JSData *(JSNumber *first, JSNumber *second) {
        return [[JSNumber alloc] initWithInt:[first intValue] % [second intValue]];
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
