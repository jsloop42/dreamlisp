//
//  TypeUtils.m
//  DreamLisp
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "TypeUtils.h"

/**
 Util functions which are used by DL type classes. This class does not include any DL types so that the DL type can call these as common methods with circular
 dependency.
 */
@implementation TypeUtils

/** Checks if the given string matches the compiled regex pattern. */
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern {
    NSArray *matches = [pattern matchesInString:string options:0 range:NSMakeRange(0, [string length])];
    return ![matches isEmpty];
}

+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block {
    NSMutableArray *acc = [NSMutableArray new];
    [array enumerateObjectsUsingBlock:^(id arg, NSUInteger idx, BOOL *stop) {
        @autoreleasepool {
            [acc addObject:block(arg)];
        }
    }];
    return acc;
}

#pragma mark Check arity

/**
 Checks if the given list satisfies the arity count. Else throws an exception.

 @param xs The arguments array.
 @param arity The argument count.
 */
+ (void)checkArity:(NSMutableArray *)xs arity:(NSInteger)arity {
    return [self checkArityCount:[xs count] arity:arity];
}

+ (void)checkArity:(NSMutableArray *)xs arities:(NSArray *)arities {
    NSInteger count = [xs count];
    if (![arities containsObject:@(count)]) [[[DLError alloc] initWithFormat:ArityAnyError, [arities componentsJoinedByString:@" | "], count] throw];
}

/**
 Check arity with arguments array and caller function. If it does not match, throws an error.

 @param xs The arguments array.
 @param arity Expected arity.
 @param fnName The caller function name
 */
+ (void)checkArity:(NSMutableArray *)xs arity:(NSInteger)arity fnName:(NSString *)fnName {
    return [self checkArityCount:[xs count] arity:arity fnName:fnName];
}

/**
 The given array should satisfy the arity count for the given predicate.
 @param xs An array contains arguments.
 @param arity The arity count which needs to be checked against.
 @param predicate The condition that should be met.
 @throw @c ArityError
 */
+ (void)checkArity:(NSMutableArray *)xs arity:(NSInteger)arity predicate:(enum ArityPredicate)predicate {
    NSUInteger count = [xs count];
    switch (predicate) {
        case ArityPredicateEq:
            if (count != arity) [[[DLError alloc] initWithFormat:ArityError, arity, count] throw];
            break;
        case ArityPredicateGreaterThan:
            if (count <= arity) [[[DLError alloc] initWithFormat:ArityGreaterThanError, arity, count] throw];
            break;
        case ArityPredicateGreaterThanOrEq:
            if (count < arity) [[[DLError alloc] initWithFormat:ArityGreaterThanOrEqualError, arity, count] throw];
            break;
        case ArityPredicateLessThan:
            if (count >= arity) [[[DLError alloc] initWithFormat:ArityLessThanError, arity, count] throw];
            break;
        case ArityPredicateLessThanOrEq:
            if (count > arity) [[[DLError alloc] initWithFormat:ArityLessThanOrEqualError, arity, count] throw];
            break;
        case ArityPredicateMax:
            if (count > arity) [[[DLError alloc] initWithFormat:ArityMaxError, arity, count] throw];
            break;
        case ArityPredicateMin:
            if (count < arity) [[[DLError alloc] initWithFormat:ArityMinError, arity, count] throw];
            break;
        case ArityPredicateMultiple:
            if (count % arity != 0) [[[DLError alloc] initWithFormat:ArityMultipleError, arity, count] throw];
            break;
        case ArityPredicateOdd:
            if (count % 2 == 0) [[[DLError alloc] initWithFormat:ArityOddError, count] throw];
            break;
        default:
            break;
    }
}

#pragma mark check arity count

+ (void)checkArityCount:(NSInteger)count arity:(NSInteger)arity {
    if (count != arity) [[[DLError alloc] initWithFormat:ArityError, arity, count] throw];
}

/**
  Check arity with count and caller function. If it does not match, throws an error.

  @param count The obtained argument count.
  @param arity Expected arity.
  @param fnName The caller function name
 */
+ (void)checkArityCount:(NSInteger)count arity:(NSInteger)arity fnName:(NSString *)fnName {
    if (count != arity) [[[DLError alloc] initWithFormat:ArityError, arity, count] throw];
}

#pragma mark Check index bounds

/**
 Checks if the given index is within the array bounds.

 @param xs An array.
 @param index The index to check.
 @throw @c IndexOutOfBounds exception.
 */
+ (void)checkIndexBounds:(NSMutableArray *)xs index:(NSInteger)index {
    NSUInteger count = [xs count];
    [self checkIndexBoundsCount:count index:index];
}

#pragma mark Check index bounds count

/**
 Checks if the given index is within [0, count].

 @param count The count to check against.
 @param index The index to check.
 @throw @c IndexOutOfBounds exception.
 */
+ (void)checkIndexBoundsCount:(NSInteger)count index:(NSInteger)index {
    if (index < 0) {
        [[[DLError alloc] initWithFormat:IndexOutOfBounds, index, 0] throw];
    } else if (index >= count) {
        [[[DLError alloc] initWithFormat:IndexOutOfBounds, index, count] throw];
    }
}

+ (void)checkIndexBoundsCount:(NSInteger)count startIndex:(NSInteger)start endIndex:(NSInteger)end {
    if (start < 0) {
        [[[DLError alloc] initWithFormat:IndexOutOfBounds, start, 0] throw];
    } else if (start > end) {
        [[[DLError alloc] initWithFormat:IndexOutOfBounds, start, end] throw];
    }
    if (end >= count) {
        [[[DLError alloc] initWithFormat:IndexOutOfBounds, end, count] throw];
    }
}

@end

NSInteger sortAscending(id obj1, id obj2, void *context) {
    NSInteger h1 = [obj1 sortValue];
    NSInteger h2 = [obj2 sortValue];
    if (h1 == h2) return NSOrderedSame;
    return h1 < h2 ? NSOrderedAscending : NSOrderedDescending;
}

NSInteger sortDescending(id obj1, id obj2, void *context) {
    NSInteger h1 = [obj1 sortValue];
    NSInteger h2 = [obj2 sortValue];
    if (h1 == h2) return NSOrderedSame;
    return h1 > h2 ? NSOrderedAscending : NSOrderedDescending;
}
