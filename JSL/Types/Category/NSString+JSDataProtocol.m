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
    return [[object classForCoder] isEqual:@"NSString"];
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

- (NSInteger)sortValue {
    return [self hash];
}

@end
