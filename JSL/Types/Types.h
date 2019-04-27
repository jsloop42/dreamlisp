//
//  Types.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Env.h"
#import "Utils.h"
#import "Reader.h"
#import "Printer.h"
#import "Logger.h"
#import "NSArray+JSDataProtocol.h"
#import "NSDecimalNumber+JSDataProtocol.h"
#import "NSMapTable+JSHashMap.h"
#import "NSMutableArray+JSList.h"
#import "NSNumber+JSDataProtocol.h"
#import "NSString+JSDataProtocol.h"
#import "TypeUtils.h"

NS_ASSUME_NONNULL_BEGIN

@class Env;

@protocol JSDataProtocol <NSObject, NSCopying, NSMutableCopying>
@property (nonatomic, readonly) NSString *dataType;
@property (nonatomic, readwrite) id value;
@property (nonatomic, readwrite) JSData *meta;
- (BOOL)hasMeta;
@end

@protocol JSHashable <JSDataProtocol>
- (NSUInteger)hash;
@end

@protocol JSEquatable <JSDataProtocol>
- (BOOL)isEqual:(JSData *)object;
@end

@interface JSData : NSObject <JSDataProtocol, JSHashable, JSEquatable>
@end

@interface JSString : JSData
@property (nonatomic, readwrite, copy) NSString *value;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithFormat:(NSString *)format, ...;
- (instancetype)initWithContentsOfFile:(NSString *)filePath;
- (instancetype)initWithCString:(const char *)string;
- (instancetype)initWithMeta:(JSData *)meta string:(JSString *)string;
- (BOOL)isEqual:(JSString *)string;
@end

@interface JSKeyword : JSData
@property (nonatomic, readwrite, copy) NSString *value;
+ (BOOL)isKeyword:(id)string;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithKeyword:(NSString *)string;
- (instancetype)initWithEncodedKeyword:(NSString *)keyword;
- (instancetype)initWithMeta:(JSData *)meta keyword:(JSKeyword *)keyword;
- (NSString *)string;
- (NSString *)encoded;
- (NSString *)decoded;
- (BOOL)isEqual:(JSKeyword *)keyword;
@end

@interface JSSymbol: JSData
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (instancetype)initWithMeta:(JSData *)meta symbol:(JSSymbol *)symbol;
- (NSString *)name;
- (BOOL)isEqual:(JSSymbol *)sym;
@end

@interface JSHashMap : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)init;
- (instancetype)initWithMapTable:(NSMapTable *)table;
- (instancetype)initWithArray:(NSArray *)array;
- (instancetype)initWithMeta:(JSData *)meta hashmap:(JSHashMap *)hashmap;
- (JSData *)objectForKey:(id)key;
- (void)setObject:(JSData *)object forKey:(id)key;
- (NSUInteger)count;
- (NSMapTable *)value;
- (void)setValue:(NSMapTable *)table;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (BOOL)isEqual:(JSData *)hashmap;
@end

@interface JSList : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)init;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(JSData *)meta list:(JSList *)list;
- (void)add:(JSData *)object;
- (void)add:(JSData *)object atIndex:(NSUInteger)index;
- (void)remove:(JSData *)object;
- (void)removeAtIndex:(NSUInteger)index;
- (void)setValue:(NSMutableArray *)aArray;
- (NSMutableArray *)value;
- (NSUInteger)count;
- (JSData *)first;
- (JSData *)second;
- (JSData *)rest;
- (JSData *)last;
- (JSData *)dropLast;
- (JSData *)nth:(NSInteger)n;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (BOOL)isEmpty;
- (BOOL)isEqual:(JSList *)list;
- (JSList *)reverse;
- (JSList *)drop:(NSInteger)n;
@end

@interface JSVector: JSList
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(JSData *)meta vector:(JSVector *)vector;
- (BOOL)isEqual:(JSVector *)vector;
- (JSList *)list;
@end

@interface JSNumber : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)initWithDouble:(double)num;
- (instancetype)initWithInt:(int)number;
- (instancetype)initWithInteger:(NSUInteger)number;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithNumber:(NSDecimalNumber *)number;
- (instancetype)initWithMeta:(JSData *)meta number:(JSNumber *)number;
- (BOOL)isEqual:(JSNumber *)num;
- (double)doubleValue;
- (int)intValue;
- (NSUInteger)integerValue;
- (BOOL)isDouble;
- (NSDecimalNumber *)value;
- (NSString *)string;
@end

@interface JSAtom : JSData
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithData:(JSData *)data;
- (instancetype)initWithMeta:(JSData *)meta atom:(JSAtom *)atom;
- (void)setValue:(JSData *)data;
- (JSData *)value;
@end

@interface JSNil : JSData
@end

@interface JSBool : JSData
- (instancetype)initWithBool:(BOOL)flag;
- (instancetype)initWithJSBool:(JSBool *)object;
- (BOOL)value;
- (BOOL)isEqual:(JSBool *)boolean;
@end

@interface JSFunction: JSData
// ([JSData]) -> JSData
@property (nonatomic, copy) JSData *(^fn)(NSMutableArray *);
@property (nonatomic, readwrite, copy, nullable) JSData *ast;
// [Symbols]
@property (nonatomic, readwrite, retain, nullable) NSMutableArray *params;
@property (nonatomic, readwrite, copy, nullable) Env *env;
@property (nonatomic, readwrite, assign, getter=isMacro) BOOL macro;
@property (nonatomic, readwrite, copy, nullable) JSData *meta;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithAst:(JSData *)ast params:(NSMutableArray *)params env:(Env *)env macro:(BOOL)isMacro meta:(JSData * _Nullable)meta
                         fn:(JSData *(^)(NSMutableArray *))fn;
- (instancetype)initWithFn:(JSData * (^)(NSMutableArray *))fn;
- (instancetype)initWithMacro:(JSFunction *)func;
- (instancetype)initWithMeta:(JSData *)meta func:(JSFunction *)func;
- (instancetype)initWithFunction:(JSFunction *)function;
- (JSData *)apply;
- (JSData *)apply:(NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
