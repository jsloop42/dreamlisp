//
//  Utils.m
//  DreamLisp
//
//  Created by Jaseem V V on 06/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLUtils.h"

static DLCacheTable *_cache;
static BOOL _isCacheEnabled;

#pragma mark - CacheKey

@interface DLCacheKey : NSObject
@property (nonatomic, readwrite) id<DLDataProtocol> key;
@property (nonatomic, readwrite) BOOL isNative;
+ (instancetype)fromKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative;
- (instancetype)initWithKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative;
@end

@implementation DLCacheKey

+ (instancetype)fromKey:(id<DLDataProtocol>)key isNative:(BOOL)isNative {
    return [[DLCacheKey alloc] initWithKey:key isNative:isNative];
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

@implementation DLUtils

+ (void)initialize {
    if (self == [self class]) {
        _cache = [DLCacheTable new];
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
        res = [[self cache] objectForKey:[DLCacheKey fromKey:object isNative:isNative]];
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
        [[[DLError alloc] initWithFormat:DLDataTypeMismatch, @"'sequence'", [object dataTypeName]] throw];
    }
    if (_isCacheEnabled) [[self cache] setObject:res forKey:[DLCacheKey fromKey:object isNative:isNative]];
    return res;
}

+ (DLNumber *)rand:(DLNumber *)upperBound {
    uint32_t n = [upperBound intValue] + 1;
    return [[DLNumber alloc] initWithInt:arc4random_uniform(n)];
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
        id<DLDataProtocol> elem;
        NSMutableArray *arr = [list value];
        for (elem in arr) {
            if (![DLList isKindOfList:elem]) {
                [[[DLError alloc] initWithFormat:DLDataTypeMismatch, @"'sequence'", [elem dataTypeName]] throw];
                break;
            }
            [hm fromArray:[elem value]];
        }
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
        xs = (DLVector *)[xs addObject:[[DLVector alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
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

+ (NSMutableArray *)hashMapToVectorArray:(DLHashMap *)hashMap {
    NSArray *allKeys = [hashMap allKeys];
    id<DLDataProtocol> key = nil;
    NSMutableArray *ret = [NSMutableArray new];
    for (key in allKeys) {
        [ret addObject:[[DLVector alloc] initWithArray:[@[key, [hashMap objectForKey:key]] mutableCopy]]];
    }
    return ret;
}

+ (DLHashMap *)convertKeywordKeysToString:(DLHashMap *)hashmap {
    @autoreleasepool {
        DLHashMap *hm = [DLHashMap new];
        NSArray *allKeys = [hashmap allKeys];
        NSUInteger len = [allKeys count];
        NSUInteger i = 0;
        DLKeyword *key = nil;
        for (i = 0; i < len; i++) {
            key = [allKeys objectAtIndex:i];
            [hm setObject:[hashmap objectForKey:key] forKey:[key string]];
        }
        return hm;
    }
}

+ (DLHashMap *)errorToHashMap:(NSError *)error {
    DLHashMap *hm = [DLHashMap new];
    [hm setObject:[DLString stringWithString:error.description] forKey:[DLKeyword keywordWithString:@"desc"]];
    [hm setObject:[[DLNumber alloc] initWithInteger:error.code] forKey:[DLKeyword keywordWithString:@"code"]];
    return hm;
}

/** Converts a hash map to @c NSMutableDictionary */
+ (NSMutableDictionary *)dictionaryFromHashMap:(DLHashMap *)hashMap {
    NSMutableDictionary *dict = [NSMutableDictionary new];
    id<DLDataProtocol> key = nil;
    NSMapTable *table = [hashMap value];
    NSArray *allKeys = [hashMap allKeys];
    for (key in allKeys) {
        [dict setObject:[table objectForKey:key] forKey:key];
    }
    return dict;
}

+ (NSMutableDictionary *)mapTableToDictionary:(NSMapTable *)table {
    NSMutableDictionary *dict = [NSMutableDictionary new];
    NSString *key = nil;
    NSArray *allKeys = [table allKeys];
    for (key in allKeys) {
        [dict setObject:[(DLString *)[table objectForKey:key] value] forKey:key];
    }
    return dict;
}

+ (id)convertFromDLTypeToFoundationType:(id<DLDataProtocol>)value {
    Protocol *dlDataProtocol = objc_getProtocol("DLDataProtocol");
    id val = @"";
    if ([DLNil isNil:value]) {
        val = @"";
    } else if ([DLKeyword isKeyword:value]) {
        val = [(DLKeyword *)value string];
    } else if ([DLHashMap isHashMap:value]) {
        val = [self hashMapToFoundationType:value];
    } else if ([DLList isKindOfList:value]) {
        NSMutableArray *xs = [(DLList *)value value];
        id<DLDataProtocol> elem = nil;
        NSMutableArray *list = [NSMutableArray new];
        for (elem in xs) {
            [list addObject:[self convertFromDLTypeToFoundationType:elem]];
        }
        val = list;
    } else if ([DLAtom isAtom:value]) {
        val = [self convertFromDLTypeToFoundationType:[(DLAtom *)value value]];
    } else if ([DLNumber isNumber:value]) {
        DLNumber *num = (DLNumber *)value;
        val = [num value];
    } else if ([DLBool isBool:value]) {
        BOOL flag = (BOOL)[value value];
        val = flag ? @YES : @NO;
    } else if ([DLString isString:value]) {
        val = [value value];
    } else if ([value conformsToProtocol:dlDataProtocol]) {
        val = [value value];
    } else {
        val = value;
    }
    return val;
}

/** Converts the given hash map to Foundation data type, useful for JSON encoding. */
+ (NSMutableDictionary *)hashMapToFoundationType:(DLHashMap *)hashMap {
    NSMutableDictionary *dict = [NSMutableDictionary new];
    NSArray *allKeys = [hashMap allKeys];
    NSUInteger len = [allKeys count];
    NSUInteger i = 0;
    id<DLDataProtocol> aKey = nil;
    id<DLDataProtocol> aVal = nil;
    id key = nil;
    id val = nil;
    for (i = 0; i < len; i++) {
        aKey = [allKeys objectAtIndex:i];
        key = [[self convertFromDLTypeToFoundationType:aKey] description];
        aVal = [hashMap objectForKey:aKey];
        val = [self convertFromDLTypeToFoundationType:aVal];
        [dict setObject:val forKey:key];
    }
    return dict;
}

/** Check if the given number is represents a BOOL value. */
+ (BOOL)isBoolNumber:(NSNumber *)num {
    CFTypeID boolID = CFBooleanGetTypeID();
    CFTypeID numID = CFGetTypeID((__bridge CFTypeRef)(num));
    return numID == boolID;
}

/** Converts the given Foundation type to DL type. Used mainly in JSON de-serialization. */
+ (id<DLDataProtocol>)convertFromFoundationTypeToDLType:(id)value {
    id<DLDataProtocol> val = nil;
    if ([value isKindOfClass:[NSNull class]]) {
        val = [DLNil new];
    } else if ([value isKindOfClass:[NSMutableDictionary class]]) {
        val = [self dictionaryToDLType:value];
    } else if ([value isKindOfClass:[NSMutableArray class]] || [value isKindOfClass:[NSArray class]]) {
        NSArray *xs = (NSArray *)value;
        id elem = nil;
        DLVector *vec = [DLVector new];  /* It's better to use a vector instead of a list so that any potential code evaluation does not happen. */
        for (elem in xs) {
            [vec appendObject:[self convertFromFoundationTypeToDLType:elem]];
        }
        val = vec;
    } else if ([value isKindOfClass:[NSNumber class]]) {
        NSNumber *n = (NSNumber *)value;
        BOOL doesRepresentBool = [self isBoolNumber:n];
        if (doesRepresentBool) {
            CFBooleanRef boolRef = (__bridge CFBooleanRef)n;
            Boolean flag = CFBooleanGetValue(boolRef);
            val = [[DLBool alloc] initWithBool:flag];
        } else {
            val = [[DLNumber alloc] initWithString:[n stringValue]];
        }
    } else if ([value isKindOfClass:[NSString class]]) {
        val = [DLString stringWithString:value];
    } else {
        val = [DLString stringWithString:[value description]];
    }
    return val;
}

+ (DLHashMap *)dictionaryToDLType:(NSMutableDictionary *)dict {
    DLHashMap *hm = [DLHashMap new];
    NSArray *allKeys = [dict allKeys];
    NSUInteger len = [allKeys count];
    NSUInteger i = 0;
    id aKey = nil;
    id aVal = nil;
    id<DLDataProtocol> key = nil;
    id<DLDataProtocol> val = nil;
    for (i = 0; i < len; i++) {
        aKey = [allKeys objectAtIndex:i];
        key = [self convertFromFoundationTypeToDLType:aKey];
        aVal = [dict objectForKey:aKey];
        val = [self convertFromFoundationTypeToDLType:aVal];
        [hm setObject:val forKey:key];
    }
    return hm;
}

/** Serializes the given JSON string into a @c DLHashMap. */
+ (DLHashMap *)decodeJSON:(DLString *)string {
    NSString *str = [string value];
    NSData *data = [str dataUsingEncoding:NSUTF8StringEncoding];
    return [self decodeJSONFromData:data];
}

+ (DLHashMap *)decodeJSONFromData:(NSData *)data {
    NSError *err = nil;
    NSMutableDictionary *dict = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingMutableContainers error:&err];
    if (err) [[[DLError alloc] initWithFormat:DLJSONParseError, err.description] throw];
    return [self dictionaryToDLType:dict];
}

/** Serializes an @c DLHashMap to JSON string. */
+ (DLString *)encodeJSON:(DLHashMap *)hashMap {
    NSMutableDictionary *fdict = [self hashMapToFoundationType:hashMap];
    NSData *jsonData = [self encodeDictionaryToJSONData:fdict];
    NSString *jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    return [[DLString alloc] initWithString:jsonString];
}

+ (NSData *)encodeDictionaryToJSONData:(NSMutableDictionary *)dict {
    NSError *err = nil;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:dict options:NSJSONWritingSortedKeys error:&err];
    if (err) [[[DLError alloc] initWithFormat:DLJSONParseError, err.description] throw];
    return jsonData;
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
    if (![string isMutable]) [[[DLError alloc] initWithFormat:DLIsImmutableError, [string dataTypeName]] throw];
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

+ (NSString *)httpMethodTypeToString:(DLKeyword *)methodType {
    NSString *type = [methodType string];
    if ([type isEqualToString:@"get"]) return @"GET";
    if ([type isEqualToString:@"post"]) return @"POST";
    if ([type isEqualToString:@"put"]) return @"PUT";
    if ([type isEqualToString:@"patch"]) return @"PATCH";
    return @"DELETE";
}

+ (NSString *)promptWithModule:(NSString *)moduleName {
    return [[NSString alloc] initWithFormat:@"λ %@> ", moduleName];
}

+ (DLCacheTable *)cache {
    return _cache;
}

@end
