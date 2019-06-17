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
    } else {
        [[[JSError alloc] initWithFormat:DataTypeMismatch, @"'sequence'", [object dataTypeName]] throw];
    }
    [[self cache] setObject:res forKey:[CacheKey fromKey:object isNative:isNative]];
    return res;
}

+ (CacheTable *)cache {
    return _cache;
}

@end
