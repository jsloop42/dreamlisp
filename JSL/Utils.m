//
//  Utils.m
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Utils.h"

@implementation Utils

/** Checks if the given string matches the compiled regex pattern. */
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern {
    NSArray *matches = [pattern matchesInString:string options:0 range:NSMakeRange(0, [string length])];
    return ![matches isEmpty];
}

/** Checks if the given string matches the string pattern. */
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern {
    NSRange range = [string rangeOfString:pattern options:NSRegularExpressionSearch range:NSMakeRange(0, [string length])];
    return range.location != NSNotFound;
}

/** Returns an array of containing all the matches. */
+ (NSArray<NSTextCheckingResult *> *)matchesInString:(NSString *)string withExpression:(NSRegularExpression *)pattern {
    return [pattern matchesInString:string options:0 range:NSMakeRange(0, [string length])];
}

/** Returns current timestamp in millisecond. */
+ (double)timestamp {
    return (double)(NSTimeInterval)[[NSDate date] timeIntervalSince1970] * 1000;
}

+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object {
    NSMutableArray *res = [NSMutableArray new];
    if ([JSList isKindOfList:object]) {
        res = [(JSList *)object value];
    } else if ([JSString isString:object]) {
        NSString *str = [(JSString *)object value];
        NSUInteger len = [str count];
        NSUInteger i = 0;
        for (i = 0; i < len; i++) {
            [res addObject:[str substringWithRange:NSMakeRange(i, 1)]];
        }
    } else {
        [[[JSError alloc] initWithFormat:DataTypeMismatch, @"'list', 'vector' or 'string'", [object dataTypeName]] throw];
    }
    return res;
}

@end
