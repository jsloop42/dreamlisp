//
//  TypeUtils.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "TypeUtils.h"

@implementation TypeUtils

+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block {
    NSMutableArray *acc = [NSMutableArray new];
    [array enumerateObjectsUsingBlock:^(id arg, NSUInteger idx, BOOL *stop) {
        [acc addObject:block(arg)];
    }];
    return acc;
}

/**
 Checks if the given list satisfies the arity count. Else throws an exception.

 @param xs An array.
 @param arity The argument count.
 */
+ (void)checkArity:(NSMutableArray *)xs arity:(NSUInteger)arity {
    return [self checkArityCount:[xs count] arity:arity];
}

+ (void)checkArityCount:(NSUInteger)count arity:(NSUInteger)arity {
    if (count != arity) [[[JSError alloc] initWithFormat:ArityError, arity, count] throw];
}

+ (void)checkArity:(NSMutableArray *)xs arities:(NSArray *)arities {
    NSInteger count = [xs count];
    if (![arities containsObject:@(count)]) [[[JSError alloc] initWithFormat:ArityAnyError, [arities componentsJoinedByString:@" | "], count] throw];
}

/**
 Checks if the given index is within the array bounds.

 @param xs An array.
 @param index The index to check.
 @throw @c IndexOutOfBounds exception.
 */
+ (void)checkIndexBounds:(NSMutableArray *)xs index:(NSUInteger)index {
    NSUInteger count = [xs count];
    if (index < 0 || index >= count) {
        JSError *info = [[JSError alloc] initWithFormat:IndexOutOfBounds, index, count];
        [info throw];
    }
}

@end
