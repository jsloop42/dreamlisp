//
//  NSMapTable+JSHashMap.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSMapTable+JSHashMap.h"

@implementation NSMapTable (JSHashMap)

+ (NSString *)dataType {
    return @"NSMapTable";
}

- (NSString *)dataType {
    return @"NSMapTable";
}

/** Returns all keys present in the `NSMapTable`. */
- (NSArray *)allKeys {
    NSMutableArray *keys = [NSMutableArray new];
    NSEnumerator *iter = [self keyEnumerator];
    id key = nil;
    while ((key = [iter nextObject])) {
        [keys addObject:key];
    }
    return [keys copy];
}

/** Returns all objects present in the `NSMapTable`. */
- (NSArray *)allObjects {
    NSMutableArray *objects = [NSMutableArray new];
    NSEnumerator *iter = [self objectEnumerator];
    id obj = nil;
    while ((obj = [iter nextObject])) {
        [objects addObject:obj];
    }
    return [objects copy];
}

/** Returns a new `NSMapTable` with the contents added. */
- (NSMapTable *)assoc:(NSMapTable *)table {
    NSMapTable *aTable = [self mutableCopy];
    NSArray *keys = [table allKeys];
    NSUInteger len = [keys count];
    NSUInteger i = 0;
    id key = nil;
    for (i = 0; i < len; i++) {
        key = keys[i];
        [aTable setObject:[table objectForKey:key] forKey:key];
    }
    return aTable;
}

/** Returns a new `NSMapTable` with the objects for the given keys removed. */
- (NSMapTable *)dissoc:(NSArray *)keys {
    NSMapTable *aTable = [self mutableCopy];
    NSUInteger i = 0;
    NSUInteger len = [keys count];
    for(i = 0; i < len; i++) {
        [aTable removeObjectForKey:keys[i]];
    }
    return aTable;
}

/** Checks if the given key exists. */
- (BOOL)containsKey:(id)key {
    return [self objectForKey:key] != nil;
}

/** Merge key value pairs from the given table */
- (void)merge:(NSMapTable *)table {
    NSEnumerator *iter = [table keyEnumerator];
    id key = nil;
    while ((key = [iter nextObject])) {
        [self setObject:[table objectForKey:key] forKey:key];
    }
}

/** Updates key and value. If existing key is present, it is removed and the current key is added so that any properties updated in current key is reflected. */
- (void)updateObject:(id)object forKey:(id)key {
    [self removeObjectForKey:key];
    [self setObject:object forKey:key];
}

@end
