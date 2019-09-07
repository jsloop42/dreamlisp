//
//  Utils.m
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "Utils.h"

static CacheTable *_cache;
static BOOL _isCacheEnabled;

#pragma mark - CacheKey

@interface CacheKey : NSObject
@property (nonatomic, readwrite) id<DLDataProtocol> key;
@property (nonatomic, readwrite) BOOL isNative;
+ (instancetype)fromKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative;
- (instancetype)initWithKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative;
@end

@implementation CacheKey

+ (instancetype)fromKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative {
    return [[CacheKey alloc] initWithKey:key isNative:isNative];
}

- (instancetype)initWithKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative {
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
        [self enableCache];
    }
}

+ (void)enableCache {
    _isCacheEnabled = YES;
}

+ (void)disableCache {
    _isCacheEnabled = NO;
}

+ (BOOL)isCacheEnabled {
    return _isCacheEnabled;
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

+ (NSMutableArray *)toArray:(id<DLDataProtocol>)object {
    return [self toArray:object isNative:NO];
}

/**
 Converts the object to an array. This method is memoized. Setting @c isNative will convert elements to DL type instead of internal Objective-C type. This is
 required for strings only.*/
+ (NSMutableArray *)toArray:(id<DLDataProtocol>)object isNative:(BOOL)isNative {
    NSMutableArray *res = nil;
    if (_isCacheEnabled) {
        res = [[self cache] objectForKey:[CacheKey fromKey:object isNative:isNative]];
        if (res) return res;
    }
    res = [NSMutableArray new];
    if ([DLList isKindOfList:object]) {
        res = [(DLList *)object value];
    } else if ([DLString isString:object]) {
        res = [self stringToArray:object isNative:isNative];
    } else if ([DLHashMap isHashMap:object]) {
        if (isNative) {
            res = [[self addObjectsToVector:[[DLVector alloc] initWithArray:res] fromHashMap:object] value];
        } else {
            res = [self hashMapToArray:object];
        }
    } else {
        [[[DLError alloc] initWithFormat:DataTypeMismatch, @"'sequence'", [object dataTypeName]] throw];
    }
    if (_isCacheEnabled) [[self cache] setObject:res forKey:[CacheKey fromKey:object isNative:isNative]];
    return res;
}

#pragma mark - List

+ (DLList *)addObjectsToList:(DLList *)list fromList:(DLList *)aList {
    @autoreleasepool {
        DLList *xs = [list copy];
        [[xs value] addObjectsFromArray:[aList value]];
        return xs;
    }
}

+ (DLList *)addObjectsToList:(DLList *)list fromVector:(DLVector *)vector {
    return [self addObjectsToList:list fromList:vector];
}

+ (DLList *)addObjectsToList:(DLList *)list fromHashMap:(DLHashMap *)hashMap {
    @autoreleasepool {
        DLList *xs = [list copy];
        NSMutableArray *arr = [xs value];
        NSArray *allKeys = [hashMap allKeys];
        id<DLDataProtocol> key = nil;
        id<DLDataProtocol> val = nil;
        for (key in allKeys) {
            val = [hashMap objectForKey:key];
            [arr addObject:[[DLList alloc] initWithArray:[@[key, val] mutableCopy]]];
        }
        return xs;
    }
}

#pragma mark - Vector

+ (DLVector *)addObjectsToVector:(DLVector *)vector fromList:(DLList *)list {
    @autoreleasepool {
        DLVector *vec = [vector copy];
        [[vec value] addObjectsFromArray:[list value]];
        return vec;
    }
}

+ (DLVector *)addObjectsToVector:(DLVector *)vector fromVector:(DLVector *)aVector {
    return [self addObjectsToVector:vector fromList:aVector];
}

+ (DLVector *)addObjectsToVector:(DLVector *)vector fromHashMap:(DLHashMap *)hashMap {
    @autoreleasepool {
        DLVector *vec = [vector copy];
        NSMutableArray *arr = [vec value];
        NSArray *allKeys = [hashMap allKeys];
        id<DLDataProtocol> key = nil;
        id<DLDataProtocol> val = nil;
        for (key in allKeys) {
            val = [hashMap objectForKey:key];
            [arr addObject:[[DLVector alloc] initWithArray:[@[key, val] mutableCopy]]];
        }
        return vec;
    }
}

#pragma mark - HashMap

/** Creates a new hash-map with contents of the given hash-map and pairs from the list. */
+ (DLHashMap *)addObjectsToHashMap:(DLHashMap *)hashMap fromList:(DLList *)list {
    @autoreleasepool {
        DLHashMap *hm = [hashMap copy];
        [hm fromArray:[list value]];
        return hm;
    }
}

/** Creates a new hash map from the given hash maps, essentially a new merged hash-map. */
+ (DLHashMap *)addObjectsToHashMap:(DLHashMap *)hashMap fromHashMap:(DLHashMap *)aHashMap {
    DLHashMap *hm = [hashMap copy];
    NSArray *allKeys = [aHashMap allKeys];
    id<DLDataProtocol> key = nil;
    for (key in allKeys) {
        [hm setObject:[aHashMap objectForKey:key] forKey:key];
    }
    return hm;
}

/** Merge the given hash maps */
+ (void)appendObjectsToHashMap:(DLHashMap *)hashMap fromHashMap:(DLHashMap *)aHashMap {
    NSArray *allKeys = [aHashMap allKeys];
    id<DLDataProtocol> key = nil;
    for (key in allKeys) {
        [hashMap setObject:[aHashMap objectForKey:key] forKey:key];
    }
}

/** Creates a list of key value pair lists from the given hash-map. */
+ (DLList *)hashMapToList:(DLHashMap *)hashMap {
    @autoreleasepool {
        NSArray *allKeys = [hashMap allKeys];
        DLList *xs = [DLList new];
        id<DLDataProtocol> key = nil;
        for (key in allKeys) {
            [xs addObject:[[DLList alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
        }
        return xs;
    }
}

/** Creates a vector of key value pair vectors from the given hash-map. */
+ (DLVector *)hashMapToVector:(DLHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    DLVector *xs = [DLVector new];
    id<DLDataProtocol> key = nil;
    for (key in allKeys) {
        [xs addObject:[[DLVector alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
    }
    return xs;
}

/** Creates an array of key value pair array from the given hash-map. */
+ (NSMutableArray *)hashMapToArray:(DLHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    NSMutableArray *res = [NSMutableArray new];
    NSMutableArray *kv = nil;
    id<DLDataProtocol> key = nil;
    for (key in allKeys) {
        kv = [NSMutableArray new];
        [kv addObject:key];
        [kv addObject:[hashMap objectForKey:key]];
        [res addObject:kv];
    }
    return res;
}

+ (NSMutableArray *)hashMapToHashMapArray:(DLHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    id<DLDataProtocol> key = nil;
    NSMutableArray *ret = [NSMutableArray new];
    for (key in allKeys) {
        [ret addObject:[[DLHashMap alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
    }
    return ret;
}

#pragma mark - String

+ (NSMutableArray *)stringToArray:(DLString *)string isNative:(BOOL)isNative {
    NSMutableArray *res = [NSMutableArray new];
    NSString *str = [string value];
    NSString *subStr = nil;
    NSUInteger len = [str count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        @autoreleasepool {
            subStr = [str substringWithRange:NSMakeRange(i, 1)];
            [res addObject:(isNative ? [[DLString alloc] initWithString:subStr] : subStr)];
        }
    }
    return res;
}

+ (void)appendStringFromArray:(NSMutableArray *)array string:(DLString *)string {
    if (![string isMutable]) [[[DLError alloc] initWithFormat:IsImmutableError, [string dataTypeName]] throw];
    id elem = nil;
    for (elem in array) {
        if ([DLNumber isNumber:elem]) {
            [string appendString:[NSString stringWithFormat:@"%ld", [(DLNumber *)elem integerValue]]];
        } else if ([DLList isKindOfList:elem]) {
            id<DLDataProtocol> x = nil;
            NSMutableArray *arr = [(DLList *)elem value];
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
