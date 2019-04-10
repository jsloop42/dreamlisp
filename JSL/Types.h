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

NS_ASSUME_NONNULL_BEGIN

@class Env;

@protocol JSDataProtocol <NSObject>
@property (nonatomic, readonly) NSString *dataType;
@end

@interface JSData : NSObject <JSDataProtocol>
@end

@interface JSString : JSData<NSCopying>
@property (nonatomic, readwrite, copy) NSString *value;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithFormat:(NSString *)format, ...;
@end

@interface NSString (JSDataProtocol)
@property (nonatomic, readonly) NSString *dataType;
@end

@interface JSKeyword : JSData<NSCopying>
@property (nonatomic, readwrite, copy) NSString *value;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithKeyword:(NSString *)string;
@end

@interface JSHashMap : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)init;
- (instancetype)initWithDictionary:(NSMutableDictionary *)dictionary;
- (instancetype)initWithArray:(NSMutableArray *)array;
- (JSData *)objectForString:(NSString *)key;
- (JSData *)objectForKey:(JSString *)key;
- (NSUInteger)count;
- (NSMutableDictionary *)value;
- (NSArray *)allKeys;
- (NSArray *)allValues;
@end

@interface JSList : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)init;
- (instancetype)initWithArray:(NSArray *)list;
- (void)add:(JSData *)object;
- (void)add:(JSData *)object atIndex:(NSUInteger)index;
- (void)remove:(JSData *)object;
- (void)removeAtIndex:(NSUInteger)index;
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
@end

@interface JSVector: JSList
- (instancetype)initWithArray:(NSArray *)list;
@end

@interface NSArray (JSDataProtocol)
@property (nonatomic, readonly) NSString *dataType;
- (NSMutableArray *)map:(id (^)(id arg))block;
@end

@interface JSNumber : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)initWithFloat:(float)num;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithNumber:(NSDecimalNumber *)number;
- (BOOL)isEqual:(JSNumber *)num;
- (double)doubleValue;
- (int)intValue;
- (BOOL)isDouble;
- (NSDecimalNumber *)val;
- (NSString *)string;
@end

@interface NSNumber (JSDataProtocol)
@property (nonatomic, readonly) NSString *dataType;
@end

@interface NSDecimalNumber (JSDataProtocol)
@property (nonatomic, readonly) NSString *dataType;
@end

@interface JSSymbol: JSData
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name;
- (NSString *)name;
- (BOOL)isEqual:(JSSymbol *)sym;
@end

@interface JSAtom : JSData
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithData:(JSData *)data;
- (JSData *)value;
@end

@interface JSNil : JSData
@end

@interface JSTrue : JSData
@end

@interface JSFalse : JSData
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
- (JSData *)apply;
- (JSData *)apply:(NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
