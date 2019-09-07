//
//  NSMutableArray+DLList.m
//  DreamLisp
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "NSMutableArray+DLList.h"

@implementation NSMutableArray (DLList)

+ (BOOL)isMutableArray:(id)object {
    return [object isKindOfClass:[NSMutableArray class]];
}

+ (NSString *)dataType {
    return @"NSMutableArray";
}

- (NSString *)dataType {
    return @"NSMutableArray";
}

- (void)add:(id)object atIndex:(NSUInteger)index {
    [self insertObject:object atIndex:index];
}

- (void)update:(id)object atIndex:(NSUInteger)index {
    [self replaceObjectAtIndex:index withObject:object];
}

- (id)first {
    return [self firstObject];
}

- (id)second {
    return [self objectAtIndex:1];
}

/** Returns a new mutable array without the first element. The original array remains unchanged. */
- (NSMutableArray *)rest {
    @autoreleasepool {
        NSMutableArray *arr = [self mutableCopy];
        if (![arr isEmpty]) [arr removeObjectAtIndex:0];
        return arr;
    }
}

- (id)last {
    return [self objectAtIndex:[self count] - 1];
}

- (id)dropLast {
    @autoreleasepool {
        NSMutableArray *arr = [self mutableCopy];
        [arr removeLastObject];
        return arr;
    }
}

- (id)nth:(NSInteger)n {
    return [self objectAtIndex:n];
}

- (BOOL)isEmpty {
    return [self count] == 0;
}

- (NSMutableArray *)reverse {
    return [[[self reverseObjectEnumerator] allObjects] mutableCopy];
}

/** Drops @c n elements from the array. If n is negative, drops from last. If n is not within bounds, the array is returned as is. */
- (NSMutableArray *)drop:(NSInteger)n {
    NSMutableArray *arr = [self mutableCopy];
    NSUInteger count = [arr count];
    NSUInteger start = 0;
    NSUInteger len = 0;
    if (n < 0) {
        start = count + n;
        len = n * -1;
    } else if (n > 0) {
        start = 0;
        len = n;
    }
    if (start >= 0 && start < count && len >= 0 && len <= count) {
        [arr removeObjectsInRange:NSMakeRange(start, len)];
    }
    return arr;
}

- (id _Nullable)drop {
    id first = nil;
    if ([self count] > 0) {
        first = [self firstObject];
        [self removeObjectAtIndex:0];
    }
    return first;
}

- (NSString *)description {
    NSMutableString *str = [NSMutableString new];
    NSUInteger len = [self count];
    NSUInteger last = len - 1;
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        [str appendString:[self[i] description]];
        if (i != last) [str appendString:@" "];
    }
    return [NSString stringWithFormat:@"[%@]", str];
}

@end
