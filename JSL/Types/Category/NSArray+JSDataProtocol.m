//
//  NSArray+JSDataProtocol.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSArray+JSDataProtocol.h"
#import "TypeUtils.h"

@implementation NSArray (JSDataProtocol)

+ (NSString *)dataType {
    return @"NSArray";
}

- (NSString *)dataType {
    return @"NSArray";
}

- (id)first {
    return [self objectAtIndex:0];
}

- (id)second {
    return [self objectAtIndex:1];
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:[self mutableCopy] withBlock:block];
}

- (BOOL)isEmpty {
    return [self count] == 0;
}

@end
