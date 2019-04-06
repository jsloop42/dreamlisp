//
//  Types.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Types.h"

#pragma mark String

@implementation JSData
@synthesize dataType;
@end

@implementation JSString {
    NSString* string;
    BOOL _isKeyword;
    NSString* _dataType;
}
@synthesize isKeyword = _isKeyword;
@synthesize dataType = _dataType;

- (instancetype)init {
    self = [super init];
    if (self) {
        string = @"";
        _isKeyword = NO;
        _dataType = [self className];
    }
    return self;
}

- (instancetype)initWithString:(NSString *)str {
    self = [super init];
    if (self) {
        string = str;
        _dataType = [self className];
    }
    return self;
}

- (instancetype)initWithKeyword:(NSString *)str {
    self = [super init];
    if (self) {
        string = str;
        _isKeyword = YES;
    }
    return self;
}

- (NSString*)value {
    return string;
}

@end

#pragma mark HashMap

@implementation JSHashMap {
    NSMutableDictionary* dict;
}

@synthesize meta;

- (instancetype)init {
    self = [super init];
    if (self) {
        dict = [NSMutableDictionary new];
    }
    return self;
}

- (JSData*)valueForKey:(NSString*)key {
    return [dict valueForKey:key];
}

- (void)setValue:(JSData*)value forKey:(NSString *)key {
    [dict setValue:value forKey:key];
}

- (NSUInteger)count {
    return [dict count];
}

- (Class)dataType {
    return [self class];
}

@end

#pragma mark List

@implementation JSList {
    NSMutableArray* array;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        array = [NSMutableArray new];
    }
    return self;
}

- (instancetype)initWith:(NSArray*)list {
    self = [super init];
    if (self) {
        array = [[NSMutableArray alloc] initWithArray:list];
    }
    return self;
}

- (void)add:(JSData*)object {
    [array addObject:object];
}

- (void)add:(JSData*)object atIndex:(NSUInteger)index {
    [array insertObject:object atIndex:index];
}

- (void)remove:(JSData*)object {
    [array removeObject:object];
}

- (void)removeAtIndex:(NSUInteger)index {
    [array removeObjectAtIndex:index];
}

- (NSMutableArray*)value {
    return array;
}

- (NSUInteger)count {
    return [array count];
}

- (JSData*)first {
    return [array firstObject];
}

- (JSData*)second {
    return [array objectAtIndex:1];
}

- (JSData*)rest {
    NSArray* arr = [array objectsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(1, [array count] - 1)]];
    return [[JSList alloc] initWith:arr];
}

- (JSData*)last {
    return [array objectAtIndex:[array count] - 1];
}

@end

#pragma mark Vector

@implementation JSVector
@end

#pragma mark Number

@implementation JSNumber {
    NSNumber* n;
}

@synthesize meta;

- (instancetype)initWith:(float)num {
    self = [super init];
    if (self) {
        n = [[NSNumber alloc] initWithFloat:num];
    }
    return self;
}

- (BOOL)isEqual:(JSNumber*)num {
    return [n isEqualToNumber:[num val]];
}

- (float)value {
    return [n floatValue];
}

- (NSNumber*)val {
    return n;
}

@end

#pragma mark Symbol

@implementation JSSymbol {
    NSString* _name;
}

- (instancetype)initWithName:(NSString*)name
{
    self = [super init];
    if (self) {
        _name = name;
    }
    return self;
}

- (NSString*)name {
    return _name;
}

- (BOOL)isEqual:(JSSymbol*)sym {
    return [_name isEqualToString:[sym name]];
}

@end

#pragma mark Atom

@implementation JSAtom {
    JSData* _data;
}

- (instancetype)initWith:(JSData*) data {
    self = [super init];
    if (self) {
        _data = data;
    }
    return self;
}

- (JSData*)value {
    return _data;
}

@end

#pragma mark Nil

@implementation JSNil
@end

#pragma mark True

@implementation JSTrue
@end

#pragma mark False

@implementation JSFalse
@end
