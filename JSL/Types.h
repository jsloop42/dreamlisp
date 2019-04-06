//
//  Types.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Env.h"

NS_ASSUME_NONNULL_BEGIN

@protocol JSDataProtocol<NSObject>
@property (nonatomic, readonly) NSString *dataType;
@end

@interface JSData : NSObject<JSDataProtocol> {
}
@end

@interface JSString : JSData
@property (nonatomic, readwrite) BOOL isKeyword;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithKeyword:(NSString *)str;
- (NSString *)value;
@end

@interface JSHashMap : JSData
@property (nonatomic, readwrite) JSData *meta;
- (NSUInteger)count;
@end

@interface JSList : JSData
@property (nonatomic, readwrite) JSData *meta;
- (instancetype)initWith:(NSArray *)list;
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
@end

@interface JSVector: JSList
@end

@interface JSNumber : JSData
@property (nonatomic, readwrite) JSData *meta;
- (instancetype)initWith:(float)num;
- (BOOL)isEqual:(JSNumber *)num;
- (float)value;
- (NSNumber *)val;
@end

@interface JSSymbol: JSData
- (NSString *)name;
- (BOOL)isEqual:(JSSymbol *)sym;
@end

@interface JSAtom : JSData
- (instancetype)initWith:(JSData *)data;
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

- (instancetype)initWithAst:(JSData *)ast params:(NSMutableArray *)params env:(Env *)env isMacro:(BOOL)isMacro meta:(JSData *)meta
                         fn:(JSData *(^)(NSMutableArray *))fn;
- (instancetype)initWithIsMacro:(BOOL)isMacro func:(JSFunction *)func;
- (instancetype)initWithMeta:(JSData *)meta func:(JSFunction *)func;
- (JSData *)apply;
- (JSData *)apply:(NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
