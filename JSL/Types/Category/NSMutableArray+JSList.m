//
//  NSMutableArray+JSList.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSMutableArray+JSList.h"

@implementation NSMutableArray (JSList)

-(NSString *)dataType {
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

- (NSMutableArray *)rest {
    NSMutableArray *arr = [self mutableCopy];
    [arr removeObjectAtIndex:0];
    return arr;
}

- (id)last {
    return [self objectAtIndex:[self count] - 1];
}

- (id)dropLast {
    NSMutableArray *arr = [self mutableCopy];
    [arr removeLastObject];
    return arr;
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

- (NSMutableArray * _Nullable)drop:(NSInteger)n {
    NSMutableArray *arr = [self mutableCopy];
    if (n > 0 && n <= [arr count]) {
        [arr removeObjectsInRange:NSMakeRange(0, n)];
        return arr;
    }
    return nil;
}

@end
