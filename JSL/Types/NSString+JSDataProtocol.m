//
//  NSString+JSString.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSString+JSDataProtocol.h"

@implementation NSString (JSDataProtocol)

- (NSString *)dataType {
    return @"NSString";
}

- (BOOL)isEmpty {
    return [self length] == 0;
}

- (BOOL)isNotEmpty {
    return [self length] > 0;
}

@end
