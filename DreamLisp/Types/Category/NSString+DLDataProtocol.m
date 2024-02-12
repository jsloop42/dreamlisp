//
//  NSString+DLString.m
//  DreamLisp
//
//  Created by Jaseem V V on 13/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "NSString+DLDataProtocol.h"

@implementation NSString (DLDataProtocol)

+ (BOOL)isString:(id)object {
    return [object isKindOfClass:[NSString class]];
}

+ (NSString *)dataType {
    return @"NSString";
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
