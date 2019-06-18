//
//  Utils.m
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Utils.h"

static CacheTable *_cache;

#pragma mark - CacheKey

@interface CacheKey : NSObject
@property (nonatomic, readwrite) id<JSDataProtocol> key;
@property (nonatomic, readwrite) BOOL isNative;
+ (instancetype)fromKey:(id<JSDataProtocol>)key isNative:(BOOL)isNative;
- (instancetype)initWithKey:(id<JSDataProtocol>)key isNative:(BOOL)isNative;
@end

@implementation CacheKey

+ (instancetype)fromKey:(id<JSDataProtocol>)key isNative:(BOOL)isNative {
    return [[CacheKey alloc] initWithKey:key isNative:isNative];
}

- (instancetype)initWithKey:(id<JSDataProtocol>)key isNative:(BOOL)isNative {
    self = [super init];
    if (self) {
        _key = key;
        _isNative = isNative;
    }
    return self;
}

@end

#pragma mark - Utils

@implementation Utils

+ (void)initialize {
    if (self == [self class]) {
        _cache = [CacheTable new];
    }
}

/** Checks if the given string matches the compiled regex pattern. */
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern {
    NSArray *matches = [pattern matchesInString:string options:0 range:NSMakeRange(0, [string length])];
    return ![matches isEmpty];
}

/** Checks if the given string matches the string pattern. */
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern {
    NSRange range = [string rangeOfString:pattern options:NSRegularExpressionSearch range:NSMakeRange(0, [string length])];
    return range.location != NSNotFound;
}

/** Returns an array of containing all the matches. */
+ (NSArray<NSTextCheckingResult *> *)matchesInString:(NSString *)string withExpression:(NSRegularExpression *)pattern {
    return [pattern matchesInString:string options:0 range:NSMakeRange(0, [string length])];
}

/** Returns current timestamp in millisecond. */
+ (double)timestamp {
    return (double)(NSTimeInterval)[[NSDate date] timeIntervalSince1970] * 1000;
}

+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object {
    return [self toArray:object isNative:NO];
}

/**
 Converts the object to an array. This method is memoized. Setting @c isNative will convert elements to JSL type instead of internal Objective-C type. This
 is required for strings only.*/
+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object isNative:(BOOL)isNative {
    NSMutableArray *res = [[self cache] objectForKey:[CacheKey fromKey:object isNative:isNative]];
    if (res) return res;
    res = [NSMutableArray new];
    if ([JSList isKindOfList:object]) {
        res = [(JSList *)object value];
    } else if ([JSString isString:object]) {
        NSString *str = [(JSString *)object value];
        NSString *subStr = nil;
        NSUInteger len = [str count];
        NSUInteger i = 0;
        for (i = 0; i < len; i++) {
            subStr = [str substringWithRange:NSMakeRange(i, 1)];
            [res addObject:(isNative ? [[JSString alloc] initWithString:subStr] : subStr)];
        }
    } else if ([JSHashMap isHashMap:object]) {
        if (isNative) {
            res = [[self addObjectsToVector:[[JSVector alloc] initWithArray:res] fromHashMap:object] value];
        } else {
            res = [self hashMapToArray:object];
        }
    } else {
        [[[JSError alloc] initWithFormat:DataTypeMismatch, @"'sequence'", [object dataTypeName]] throw];
    }
    [[self cache] setObject:res forKey:[CacheKey fromKey:object isNative:isNative]];
    return res;
}

#pragma mark - List

+ (JSList *)addObjectsToList:(JSList *)list fromList:(JSList *)aList {
    JSList *xs = [list copy];
    [[xs value] addObjectsFromArray:[aList value]];
    return xs;
}

+ (JSList *)addObjectsToList:(JSList *)list fromVector:(JSVector *)vector {
    return [self addObjectsToList:list fromList:vector];
}

+ (JSList *)addObjectsToList:(JSList *)list fromHashMap:(JSHashMap *)hashMap {
    JSList *xs = [list copy];
    NSMutableArray *arr = [xs value];
    NSArray *allKeys = [hashMap allKeys];
    id<JSDataProtocol> key = nil;
    id<JSDataProtocol> val = nil;
    for (key in allKeys) {
        val = [hashMap objectForKey:key];
        [arr addObject:[[JSList alloc] initWithArray:[@[key, val] mutableCopy]]];
    }
    return xs;
}

#pragma mark - Vector

+ (JSVector *)addObjectsToVector:(JSVector *)vector fromList:(JSList *)list {
    JSVector *vec = [vector copy];
    [[vec value] addObjectsFromArray:[list value]];
    return vec;
}

+ (JSVector *)addObjectsToVector:(JSVector *)vector fromVector:(JSVector *)aVector {
    return [self addObjectsToVector:vector fromList:aVector];
}

+ (JSVector *)addObjectsToVector:(JSVector *)vector fromHashMap:(JSHashMap *)hashMap {
    JSVector *vec = [vector copy];
    NSMutableArray *arr = [vec value];
    NSArray *allKeys = [hashMap allKeys];
    id<JSDataProtocol> key = nil;
    id<JSDataProtocol> val = nil;
    for (key in allKeys) {
        val = [hashMap objectForKey:key];
        [arr addObject:[[JSVector alloc] initWithArray:[@[key, val] mutableCopy]]];
    }
    return vec;
}

#pragma mark - HashMap

/** Creates a new hash-map with contents of the given hash-map and pairs from the list. */
+ (JSHashMap *)addObjectsToHashMap:(JSHashMap *)hashMap fromList:(JSList *)list {
    JSHashMap *hm = [hashMap copy];
    [hm fromArray:[list value]];
    return hm;
}

/** Creates a new hash map from the given hash maps, essentially a new merged hash-map. */
+ (JSHashMap *)addObjectsToHashMap:(JSHashMap *)hashMap fromHashMap:(JSHashMap *)aHashMap {
    JSHashMap *hm = [hashMap copy];
    NSArray *allKeys = [aHashMap allKeys];
    id<JSDataProtocol> key = nil;
    for (key in allKeys) {
        [hm setObject:[aHashMap objectForKey:key] forKey:key];
    }
    return hm;
}

/** Creates a list of key value pair lists from the given hash-map. */
+ (JSList *)hashMapToList:(JSHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    JSList *xs = [JSList new];
    id<JSDataProtocol> key = nil;
    for (key in allKeys) {
        [xs addObject:[[JSList alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
    }
    return xs;
}

/** Creates a vector of key value pair vectors from the given hash-map. */
+ (JSVector *)hashMapToVector:(JSHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    JSVector *xs = [JSVector new];
    id<JSDataProtocol> key = nil;
    for (key in allKeys) {
        [xs addObject:[[JSVector alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
    }
    return xs;
}

/** Creates a vector of key value pair vectors from the given hash-map. */
+ (NSMutableArray *)hashMapToArray:(JSHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    NSMutableArray *res = [NSMutableArray new];
    NSMutableArray *kv = [NSMutableArray new];
    id<JSDataProtocol> key = nil;
    for (key in allKeys) {
        kv = [NSMutableArray new];
        [kv addObject:key];
        [kv addObject:[hashMap objectForKey:key]];
        [res addObject:kv];
    }
    return res;
}

#pragma mark String

+ (void)appendStringFromArray:(NSMutableArray *)array string:(JSString *)string {
    if (![string isMutable]) [[[JSError alloc] initWithFormat:IsImmutableError, [string dataTypeName]] throw];
    id elem = nil;
    for (elem in array) {
        if ([JSNumber isNumber:elem]) {
            [string appendString:[NSString stringWithFormat:@"%ld", [(JSNumber *)elem integerValue]]];
        } else if ([JSList isKindOfList:elem]) {
            id<JSDataProtocol> x = nil;
            NSMutableArray *arr = [(JSList *)elem value];
            for (x in arr) {
                [self appendStringFromArray:arr string:string];
            }
        } else if ([NSString isString:elem]) {
            [string appendString:(NSString *)elem];
        } else {
            [string appendString:[elem description]];
        }
    }
}


+ (CacheTable *)cache {
    return _cache;
}

@end
