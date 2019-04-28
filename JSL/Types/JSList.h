//
//  JSList.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"
#import "TypeUtils.h"
#import "NSMutableArray+JSList.h"

NS_ASSUME_NONNULL_BEGIN

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
- (NSUInteger)hash;
- (JSList *)reverse;
- (JSList *)drop:(NSInteger)n;
@end

NS_ASSUME_NONNULL_END
