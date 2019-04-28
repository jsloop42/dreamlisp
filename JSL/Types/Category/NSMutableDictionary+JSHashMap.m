//
//  NSMutableDictionary+JSHashMap.m
//  JSL
//
//  Created by jsloop on 21/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSMutableDictionary+JSHashMap.h"

@implementation NSMutableDictionary (JSHashMap)

- (NSString *)description {
    NSMutableArray *arr = [NSMutableArray new];
    NSArray *keys = [self allKeys];
    NSUInteger len = [keys count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        [arr addObject:[[NSString alloc] initWithFormat:@"%@: %@", keys[i], [self objectForKey:keys[i]]]];
    }
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@>", NSStringFromClass([self class]), self, [arr componentsJoinedByString:@", "]];
}

@end
