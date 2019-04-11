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
    [self addArithmeticFunctions];
}

- (void)addArithmeticFunctions {
    JSData *(^add)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = ^JSData *(JSNumber *first, ...) {
        NSDecimalNumber *num = nil;
        va_list args;
        va_start(args, first);
        for (JSNumber *arg = first; arg != nil; arg = va_arg(args, JSNumber *)) {
            NSLog(@"%@", arg);
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
            NSLog(@"%@", arg);
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
            NSLog(@"%@", arg);
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
            NSLog(@"%@", arg);
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

- (NSMutableDictionary *)namespace {
    return ns;
}

@end
