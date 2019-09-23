//
//  Utils.m
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLUtils.h"

static DLCacheTable *_cache;
static BOOL _isCacheEnabled;
static Protocol *_dlDataProtocol;
static Protocol *_dlProxyProtocol;
static NSRegularExpression *_lowerCaseRegex;
static NSRegularExpression *_upperCaseRegex;
static NSRegularExpression *_setterRegex;

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
        _dlDataProtocol = objc_getProtocol("DLDataProtocol");
        _dlProxyProtocol = objc_getProtocol("DLProxyProtocol");
        _upperCaseRegex = [NSRegularExpression regularExpressionWithPattern:@"[A-Z]+" options:NSRegularExpressionUseUnixLineSeparators error:nil];
        _lowerCaseRegex = [NSRegularExpression regularExpressionWithPattern:@"[^A-Z]+" options:NSRegularExpressionUseUnixLineSeparators error:nil];
        /* Regex that matches setter string form, eg: setUniverseName: */
        _setterRegex = [NSRegularExpression regularExpressionWithPattern:@"set(\\w+)(:)" options:NSRegularExpressionUseUnixLineSeparators error:nil];
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

#pragma mark - List

+ (DLList *)addObjectsToList:(DLList *)list fromList:(DLList *)aList {
    DLList *xs = [list copy];
    [[xs value] addObjectsFromArray:[aList value]];
    return xs;
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
    DLVector *vec = [vector copy];
    [[vec value] addObjectsFromArray:[list value]];
    return vec;
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
            [string appendString:[[NSString alloc] initWithFormat:@"%ld", [(DLNumber *)elem integerValue]]];
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

+ (NSString *)capitalizeFirstChar:(NSString *)string {
    NSString *first = [string substringWithRange:NSMakeRange(0, 1)];
    return [string stringByReplacingCharactersInRange:NSMakeRange(0, 1) withString:[[NSString alloc] initWithFormat:@"%@", [first uppercaseString]]];
}

+ (NSString *)promptWithModule:(NSString *)moduleName {
    return [[NSString alloc] initWithFormat:@"λ %@> ", moduleName];
}

/*! Converts the pascal case string to camel case string. */
+ (NSString *)pascalCaseToCamelCase:(NSString *)string {
    NSString *first = [string substringToIndex:1];
    NSMutableString *str = [NSMutableString new];
    if ([first isEqualToString:[first uppercaseString]]) {  /* => String starts with a caps */
        [str appendString:[first lowercaseString]];
        [str appendString:[string substringFromIndex:1]];
    }
    return str;
}

+ (NSString *)lispCaseToPascalCase:(NSString *)string {
    
    return @"";
}

/*! Converts the given pascal case string to lisp case.  */
+ (NSString *)pascalCaseToLispCase:(NSString *)string {
    return @"";
}

+ (NSString *)lispCaseToCamelCase:(NSString *)string {
    NSArray *arr = [string componentsSeparatedByString:@"-"];
    NSUInteger i = 0;
    NSUInteger len = [arr count];
    NSMutableString *acc = [NSMutableString new];
    [acc appendString:[arr firstObject]];
    NSMutableString *str = nil;
    NSString *first = nil;
    for (i = 1; i < len; i++) {
        str = [NSMutableString stringWithString:[arr objectAtIndex:i]];
        if (![str isEmpty]) {
            first = [str substringToIndex:1];
            [str replaceCharactersInRange:NSMakeRange(0, 1) withString:[first capitalizedString]];
            [acc appendString:str];
        }
    }
    return acc;
}

/*! Converts the given camel case string to lisp case. */
+ (NSString *)camelCaseToLispCase:(NSString *)string {
    NSMutableString *str = [NSMutableString new];
    NSUInteger len = [string length];
    NSArray *lowerCaseMatches = [_lowerCaseRegex matchesInString:string options:0 range:NSMakeRange(0, len)];
    NSArray *upperCaseMatches = [_upperCaseRegex matchesInString:string options:0 range:NSMakeRange(0, len)];
    NSUInteger upperCaseMatchCount = [upperCaseMatches count];
    if (upperCaseMatchCount == 0) return string;
    NSEnumerator *enumerator = upperCaseMatches.objectEnumerator;
    NSTextCheckingResult *lcMatch;
    NSTextCheckingResult *ucMatch;
    /*
     In a string of format componentsSeparatedByNSString, the lower case regex matches:
     components, eperated, y, ring
     and the upper case regex matches
     S, B, NSS
     If there are more that two uppercase characters, then we check if it's in the prefixes list. If so, we convert them to lowercase, else, we convert the
     whole prefix into lowercase.
     */
    NSRange range;
    NSString *caps = nil;
    NSUInteger idx = 0;
    NSUInteger size = [lowerCaseMatches count];
    for (lcMatch in lowerCaseMatches) {
        ++idx;
        range = lcMatch.range;
        ucMatch = [enumerator nextObject];
        [str appendString:[string substringWithRange:range]];
        if (ucMatch) {
            [str appendString:@"-"];
            caps = [string substringWithRange:ucMatch.range];
            if ([caps length] > 1) {
                NSString *prefix = [caps substringToIndex:caps.length - 1];
//                DLTrieSearchResult *res = [DLState.shared.prefixTree search:prefix isResultInCaps:NO];
//                if (res.isExist) {
//                    [str appendString:[prefix lowercaseString]];
//                    [str appendString:@"-"];
//                    [str appendString:[[caps substringFromIndex:caps.length - 1] lowercaseString]];
//                } else {
//                    //NSString *match = res.prefixes
//                    [str appendString:[caps lowercaseString]];
//                    if (idx < size) [str appendString:@"-"];
//                }
            } else {
                [str appendString:[caps lowercaseString]];
            }
        }
    }
    return [[NSString alloc] initWithString:str];
}

/*!
 Splits the given string into individual string components
 @returns NSMutableArray
 */
+ (NSMutableArray *)splitString:(NSString *)string {
    NSUInteger len = [string length];
    NSUInteger i = 0;
    unichar uchar;
    NSMutableArray *strxs = [NSMutableArray new];
    for (i = 0; i < len; i++) {
        uchar = [string characterAtIndex:i];
        [strxs addObject:[[NSString alloc] initWithCharacters:&uchar length:1]];
    }
    return strxs;
}

#pragma mark - Network

+ (NSString *)httpMethodTypeToString:(DLKeyword *)methodType {
    NSString *type = [methodType string];
    if ([type isEqualToString:@"get"]) return @"GET";
    if ([type isEqualToString:@"post"]) return @"POST";
    if ([type isEqualToString:@"put"]) return @"PUT";
    if ([type isEqualToString:@"patch"]) return @"PATCH";
    return @"DELETE";
}

#pragma mark - Objective-C RT

/** Check if the given number is represents a BOOL value. */
+ (BOOL)isBoolNumber:(NSNumber *)num {
    CFTypeID boolID = CFBooleanGetTypeID();
    CFTypeID numID = CFGetTypeID((__bridge CFTypeRef)(num));
    return numID == boolID;
}

/** Converts the given Foundation type to DL type. Used mainly in JSON de-serialization. */
+ (id<DLDataProtocol>)convertFromFoundationTypeToDLType:(id)value {
    id<DLDataProtocol> val = nil;
    if (class_conformsToProtocol([value class], _dlDataProtocol)) return value;
    if ([value isKindOfClass:[NSMutableArray class]] || [value isKindOfClass:[NSArray class]]) {
        NSArray *xs = (NSArray *)value;
        id elem = nil;
        DLVector *vec = [DLVector new];  /* It's better to use a vector instead of a list so that any potential code evaluation does not happen. */
        for (elem in xs) {
            [vec appendObject:[self convertFromFoundationTypeToDLType:elem]];
        }
        val = vec;
    } else if ([value isKindOfClass:[NSMutableDictionary class]]) {  // TODO: check for NSMapTable
        val = [self dictionaryToDLType:value];
    } else if ([value isKindOfClass:[NSNumber class]]) {  // TODO: check for other number types like NSDecimalNumber
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
    } else if ([value isKindOfClass:[NSMutableString class]]) {
        val = [[DLString alloc] initWithMutableString:value];
    } else if ([value isKindOfClass:[NSData class]]) {
        val = [[DLData alloc] initWithData:value];
    } else if ([value isKindOfClass:[NSRegularExpression class]]) {
        val = [[DLRegex alloc] initWithRegularExpression:value];
    } else if ([value isKindOfClass:[NSNull class]]) {
        val = [DLNil new];
    } else if ([value isKindOfClass:[NSObject class]]) {
        val = [[DLObject alloc] initWithProxy:value];
    } else if (class_isMetaClass(object_getClass(value))) {
        val = [[DLClass alloc] initWithProxy:value];
    } else if (!value) {
        val = [DLNil new];
    } else {
        NSAssert(false, @"This path should not be taken");
        val = [DLString stringWithString:[value description]];
    }
    return val;
}

+ (id)convertFromDLTypeToFoundationType:(id<DLDataProtocol>)value {
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
    } else if ([value conformsToProtocol:_dlDataProtocol]) {
        val = [value value];
    } else {
        val = value;
    }
    return val;
}

/*!
 Convert the given keyword to a selector

 :init-with-string to initWithString:
 */
+ (SEL)convertKeywordToSelector:(NSMutableArray<DLKeyword *> *)keywordArr {
    DLKeyword *kwd = nil;
    NSString *kwdStr = nil;
    NSMutableString *str = [NSMutableString new];
    for (kwd in keywordArr) {
        kwdStr = [kwd string];
        [str appendString:[self lispCaseToCamelCase:kwdStr]];
        [str appendString:@":"];
    }
    return NSSelectorFromString(str);
}

/*!
 Adds selector for the method with lisp case converted into camel case.

 For a method like:
 (defmethod gen-random 'utils (start :with-max end) ())
 the selector string is
 genRandom:withMax:
 */
+ (void)updateSELForMethod:(DLMethod *)method {
    NSMutableArray *paramsList = method.params;
    NSUInteger i = 0;
    NSUInteger len = [paramsList count];
    DLMethodParam *param = nil;
    NSMutableString *selName = [NSMutableString new];
    [selName appendString:[self lispCaseToCamelCase:method.name.value]];
    if (len > 0) [selName appendString:@":"];
    for (i = 0; i < len; i++) {
        param = [paramsList objectAtIndex:i];
        if (param.selectorName) {
            [selName appendString:[self lispCaseToCamelCase:[param.selectorName string]]];
            [selName appendString:@":"];
        }
    }
    SEL sel = NSSelectorFromString(selName);
    method.selector = sel;
}

/*! Adds selector string in lisp case (default), used for printing. */
+ (void)updateSelectorStringForMethod:(DLMethod *)method {
    NSMutableString *str = [NSMutableString new];
    [str appendString:[self camelCaseToLispCase:method.name.value]];  // componentsSeparatedByString -> components-separated-by-string
    if (method.params.count > 0) [str appendString:@":"];
    DLMethodParam *param;
    for (param in method.params) {
        if (param.selectorName) {
            [str appendString:[self camelCaseToLispCase:[param.selectorName string]]];
            [str appendString:@":"];
        }
    }
    method.selectorString = [[DLString alloc] initWithMutableString:str];
}

/*!
 Updates the property attributes from the existing main values. Call this after setting value, which will then generate backing Ivar, getter, setter, type
 string.
 */
+ (void)updatePropertyAttr:(DLObjcPropertyAttr *)attr {
    NSString *attrName = [self lispCaseToCamelCase:attr.value];
    attr.name = [attrName UTF8String];
    attr.backingIvar = [[[NSString alloc] initWithFormat:@"_%s", attr.name] UTF8String];
    if (attr.hasCustomGetter) {
        NSString *getterName = [self lispCaseToCamelCase:attr.getter.value];
        attr.getterName = [getterName UTF8String];
    } else {
        attr.getterName = attr.name;
    }
    if (attr.hasCustomSetter) {
        NSString *setterName = [self lispCaseToCamelCase:attr.setter.value];
        attr.setterName = [setterName UTF8String];
    } else {
        attr.setterName = [[self toSetterName:attrName isCamelCase:YES] UTF8String];
    }
    if (!attr.type) {
        attr.type = "@";  /* Type assumes object as its default value if none given. */
    }
}

/*!
 Returns the property name corresponding to the given selector string, which is already in camel case.
 Eg: setUniverseName: gives universeName
 */
+ (NSString *)propertyNameFromSelector:(NSString *)selector {
    BOOL flag = [selector hasSuffix:@":"];
    if (!flag) return selector;  /* Not a setter string */
    NSArray *matches = [_setterRegex matchesInString:selector options:0 range:NSMakeRange(0, selector.length)];
    if (matches.count == 1) {
        NSTextCheckingResult *match = matches.firstObject;
        NSUInteger ranges = match.numberOfRanges;
        NSString *propName = nil;
        if (ranges == 3) {  /* we have a setter */
            propName = [selector substringWithRange:[match rangeAtIndex:1]];
            return [self pascalCaseToCamelCase:propName];
        }
    }
    return selector;
}

/*! Converts the given getter string to accessor var with an @c _ */
+ (NSString *)toAccessorVarFromGetter:(NSString *)string {
    if ([string count] == 0) return string;
    NSString *first = [string substringWithRange:NSMakeRange(0, 1)];
    return [string stringByReplacingCharactersInRange:NSMakeRange(0, 1) withString:[[NSString alloc] initWithFormat:@"_%@", [first lowercaseString]]];
}

/*!
 Converts the given property name to setter name with camel case. Set the @c isCamelCase if the string is already in camel case, else case conversion will be
 done. */
+ (NSString *)toSetterName:(NSString *)propName isCamelCase:(BOOL)isCamelCase {
    if ([propName isEmpty]) return propName;
    propName = isCamelCase ? propName : [self lispCaseToCamelCase:propName];
    NSString *first = [propName substringToIndex:1];
    NSMutableString *str = [NSMutableString new];
    [str appendString:@"set"];
    [str appendString:[first uppercaseString]];
    [str appendString:[propName substringFromIndex:1]];
    [str appendString:@":"];
    return str;
}

+ (DLInvocationArgument *)convertToInvocationArgument:(id<DLDataProtocol>)elem {
    DLInvocationArgument *invoArg = [DLInvocationArgument new];
    NSMutableArray *arr = [NSMutableArray new];
    invoArg.args = arr;
    if ([DLList isKindOfList:elem]) {  /* DLList, DLVector */
        DLList *xs = [(DLList *)elem value];
        [arr addObject:xs];
        invoArg.type = [xs dataType];
    } else if ([DLAtom isAtom:elem]) {
        DLAtom *atom = [(DLAtom *)elem value];
        [self convertToInvocationArgument:atom]; /* DLAtom is a container class, as such we need to unpack the contained value. */
    } else if ([DLBool isBool:elem]) {
        BOOL flag = [(DLBool *)elem value];
        NSValue *val = [NSValue value:&flag withObjCType:@encode(BOOL)];
        [arr addObject:val];
        invoArg.type = @"bool";
    } else if ([DLClass isClass:elem]) {
        [arr addObject:[(DLClass *)elem value]];
        invoArg.type = @"class";
    } else if ([DLFunction isFunction:elem]) {
        /*
         Cannot use a DLFunction as such because the function is a container for the body which is Lisp ast. So calling a DLFunction from Objective-C side of
         things is not helpful because then we require an interpreter for running that function body. Another option is then to have a compiler which will parse
         the DLFunction which is DL AST to Objective-C method.
         So if in REPL only target, we would have to define an Objective-C block and then pass that as the argument. Or define an Objective-C method and pass
         the SEL. The app with both, the compiler and the REPL, can use the compiler to convert the DLFunction into an Objective-C block (an iVar will be
         created class definition time to hold the initarg which will be a block and then a block can be defined already as defblock or given inline as DL code,
         which will be converted into a block.
         */

        // TODO: as such check if compiler is present, if so, invoke, else throw an error (with saying to use a block instead and an error code).
        // TODO: add error code for all errors thrown so that details can be read from the documentation, any design decisions and such related to the error.
        // This would provide a much better development experience than having to search text across the web.
        [[[DLError alloc] initWithFormat:DLMakeInstanceFnTypeError, [elem dataTypeName]] throw];
    } else if ([DLHashMap isHashMap:elem]) {
        [arr addObject:[(DLHashMap *)elem value]];
    } else if ([DLKeyword isKeyword:elem]) {
        /*
         DLKeyword is useful for ObjC, DL interop as one can pass them around, like example below. We can have parts of the logic in DL and some as DLOS.
         Which means, we need DLKeyword as a runtime (linked) library when compiling if one uses this, else we do not have to link against DLLib.
         (def week [:sunday :monday])
         (defclass routine (ns-object) ((day :initarg :for-day)))
         (make-instance 'routine :for-day :sunday)
         */
        [arr addObject:elem];  /* We can pass like :with-week :friday */
    } else if ([DLNil isNil:elem]) {
        [arr addObject:[(DLNil *)elem value]];
    } else if ([DLNumber isNumber:elem]) {

    }
    return invoArg;
}

#pragma mark - Build Helpers

/*!
 Save state to a binary file.
 */
//+ (BOOL)serializePrefixState:(DLState *)state toFile:(NSString *)file {
//    NSError *err;
//    NSMutableDictionary *dict = [NSMutableDictionary new];
//    [dict setObject:state forKey:@"state"];
//    NSData *data = [NSKeyedArchiver archivedDataWithRootObject:dict requiringSecureCoding:YES error:&err];
//    if (err) {
//        [DLLog error:@"Error serializing state"];
//        return NO;
//    }
//    DLFileOps *fops = [DLFileOps new];
//    [fops createFileIfNotExist:file];
//    [fops openFileForWriting:file];
//    [fops write:data];
//    return YES;
//}

/*!
 Restore the state from the serialized binary.
 */
//+ (DLState * _Nullable)deserializePrefixStateFromFile:(NSString *)file {
//    NSData *data = [[NSData alloc] initWithContentsOfFile:file];
//    NSError *err;
//    NSSet *set = [[NSSet alloc] initWithArray:@[[NSMutableDictionary class], [DLState class], [DLTrie class], [NSValue class], [NSMutableArray class], [NSString class], [NSDecimalNumber class]]];
//    //NSKeyedUnarchiver *unarchiver = [[NSKeyedUnarchiver alloc] initForReadingFromData:data error:&err];
//    //DLState *state = [unarchiver decodeObjectForKey:@"state"];
//    DLState *state = [NSKeyedUnarchiver unarchivedObjectOfClasses:set fromData:data error:&err];
//    if (err) {
//        NSLog(@"Error deserializing state data: %@", err);
//        return nil;
//    }
//    return state;
//}

/*!
 Initializes prefix state from serialized data.
 */
//+ (BOOL)initializePrefixState {
//    DLFileOps *fops = [DLFileOps new];
//    DLState *state = [self deserializePrefixStateFromFile:[[NSString alloc] initWithFormat:@"%@%@", [fops projectRoot], DLConst.prefixBinFilePathFrag]];
//    if (state) {
//        DLState.shared.prefixTree = state.prefixTree;
//        DLState.shared.userPrefixTree = state.userPrefixTree;
//        return YES;
//    }
//    return NO;
//}

/*!
 Loads the plist containing the prefix into the current state object and serialize it to a binary, which can then be loaded
 there after.
 */
//+ (BOOL)generatePrefixState {
//    DLFileOps *fops = [DLFileOps new];
//    NSString *prefixPlistPath = [[NSString alloc] initWithFormat:@"%@%@", fops.projectRoot, DLConst.prefixPlistPathFrag];
//    NSData *plistData = [[NSData alloc] initWithContentsOfFile:prefixPlistPath];
//    NSError *err;
//    NSMutableDictionary *plist = [NSPropertyListSerialization propertyListWithData:plistData options:NSPropertyListImmutable format:nil error:&err];
//    NSArray *prefixList = [plist objectForKey:@"prefixes"];
//    DLState *state = [DLState new];
//    if (prefixList.count > 0) {
//        [state initPrefixes:prefixList];
//    }
//    [self serializePrefixState:state toFile:[[NSString alloc] initWithFormat:@"%@%@", fops.projectRoot, DLConst.prefixBinFilePathFrag]];
//    return YES;
//}

+ (DLCacheTable *)cache {
    return _cache;
}

@end
