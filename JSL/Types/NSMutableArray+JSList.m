//
//  NSMutableArray+JSList.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSMutableArray+JSList.h"

@implementation NSMutableArray (JSList)

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

@end
