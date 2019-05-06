//
//  NSString+JSString.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSString+JSDataProtocol.h"

@implementation NSString (JSDataProtocol)

+ (BOOL)isString:(id)object {
    return [NSStringFromClass([object classForCoder]) isEqual:@"NSString"] ? YES : NO;
}

- (NSString *)dataType {
    return @"NSString";
}

- (BOOL)isEmpty {
    return [self length] == 0;
}

- (BOOL)isNotEmpty {
    return [self length] > 0;
}

- (NSUInteger)count {
    return [self length];
}

- (NSString *)trim {
    return [self stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
}

@end