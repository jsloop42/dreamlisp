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

@protocol JSDataProtocol<NSObject>
@property (nonatomic, readonly) NSString *dataType;
@end

@interface JSData : NSObject<JSDataProtocol>
@end

@interface JSString : JSData
- (instancetype)init;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithFormat:(NSString *)format, ...;
- (NSString *)value;
@end

@interface JSKeyword : JSString
- (instancetype)init;
- (instancetype)initWithKeyword:(NSString *)string;
- (NSString *)value;
@end

@interface JSHashMap : JSData
@property (nonatomic, readwrite) JSData *meta;
- (instancetype)init;
- (instancetype)initWithArray:(NSMutableArray *)array;
- (NSUInteger)count;
- (NSMutableDictionary *)value;
- (NSMutableArray *)map:(id (^)(id key, id obj))block;
@end

@interface JSList : JSData
@property (nonatomic, readwrite) JSData *meta;
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
- (NSMutableArray *)map:(id (^)(id arg))block;
@end

@interface JSVector: JSList
- (instancetype)initWithArray:(NSArray *)list;
@end

@interface JSNumber : JSData
@property (nonatomic, readwrite) JSData *meta;
@property (nonatomic, assign) enum CFNumberType numberType;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithFloat:(float)num;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithNumber:(NSNumber *)number;
- (BOOL)isEqual:(JSNumber *)num;
- (CFNumberType)value;
- (NSNumber *)val;
- (CFNumberType)numberType;
- (NSString *)string;
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
@property (nonatomic, readwrite) JSData *ast;
// [Symbols]
@property (nonatomic, readwrite) NSMutableArray *params;
@property (nonatomic, readwrite) Env *env;
@property (nonatomic, readwrite) BOOL isMacro;
@property (nonatomic, readwrite) JSData *meta;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithAst:(JSData *)ast params:(NSMutableArray *)params env:(Env *)env isMacro:(BOOL)isMacro meta:(JSData *)meta
                         fn:(JSData *(^)(NSMutableArray *))fn;
- (instancetype)initWithIsMacro:(BOOL)isMacro func:(JSFunction *)func;
- (instancetype)initWithMeta:(JSData *)meta func:(JSFunction *)func;
- (JSData *)apply;
- (JSData *)apply:(NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
