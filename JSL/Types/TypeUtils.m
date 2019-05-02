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
  The given array should satisfy the arity count for the given predicate.
  @param xs An array contains arguments.
  @param arity The arity count which needs to be checked against.
  @param predicate The condition that should be met.
  @throw @c ArityError
 */
+ (void)checkArity:(NSMutableArray *)xs arity:(NSUInteger)arity predicate:(enum ArityPredicate)predicate {
    NSUInteger count = [xs count];
    switch (predicate) {
        case ArityPredicateEq:
            if (count != arity) [[[JSError alloc] initWithFormat:ArityError, arity, count] throw];
            break;
        case ArityPredicateGreaterThan:
            if (count <= arity) [[[JSError alloc] initWithFormat:ArityGreaterThanError, arity, count] throw];
            break;
        case ArityPredicateGreaterThanOrEq:
            if (count < arity) [[[JSError alloc] initWithFormat:ArityGreaterThanOrEqualError, arity, count] throw];
            break;
        case ArityPredicateLessThan:
            if (count >= arity) [[[JSError alloc] initWithFormat:ArityLessThanError, arity, count] throw];
            break;
        case ArityPredicateLessThanOrEq:
            if (count > arity) [[[JSError alloc] initWithFormat:ArityLessThanOrEqualError, arity, count] throw];
            break;
        case ArityPredicateMax:
            if (count > arity) [[[JSError alloc] initWithFormat:ArityMaxError, arity, count] throw];
            break;
        case ArityPredicateMin:
            if (count < arity) [[[JSError alloc] initWithFormat:ArityMinError, arity, count] throw];
            break;
        case ArityPredicateMultiple:
            if (count % arity != 0) [[[JSError alloc] initWithFormat:ArityMultipleError, arity, count] throw];
            break;
        case ArityPredicateOdd:
            if (count % 2 == 0) [[[JSError alloc] initWithFormat:ArityOddError, count] throw];
            break;
        default:
            break;
    }
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
