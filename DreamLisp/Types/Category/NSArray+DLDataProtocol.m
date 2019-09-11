//
//  NSArray+DLDataProtocol.m
//  DreamLisp
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "NSArray+DLDataProtocol.h"
#import "DLTypeUtils.h"

@implementation NSArray (DLDataProtocol)

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
    return [DLTypeUtils mapOnArray:[self mutableCopy] withBlock:block];
}

- (BOOL)isEmpty {
    return [self count] == 0;
}

@end
