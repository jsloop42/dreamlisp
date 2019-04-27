//
//  NSMapTable+JSHashMap.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSMapTable+JSHashMap.h"

@implementation NSMapTable (JSHashMap)

- (NSArray *)allKeys {
    NSMutableArray *arr = [NSMutableArray new];
    NSEnumerator *iter = [self keyEnumerator];
    id value = nil;
    while ((value = [iter nextObject])) {
        [arr addObject:value];
    }
    return [arr copy];
}

- (NSArray *)allObjects {
    NSMutableArray *arr = [NSMutableArray new];
    NSEnumerator *iter = [self objectEnumerator];
    id value = nil;
    while ((value = [iter nextObject])) {
        [arr addObject:value];
    }
    return [arr copy];
}

@end
