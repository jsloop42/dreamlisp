//
//  JSList.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSError.h"
#import "TypeUtils.h"
#import "NSMutableArray+JSList.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSList : NSObject <JSDataProtocol>
+ (BOOL)isList:(id)object;
+ (BOOL)isKindOfList:(id)object;
+ (JSList *)dataToList:(id<JSDataProtocol>)data;
+ (JSList *)dataToList:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)init;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta list:(JSList *)list;
- (void)add:(id<JSDataProtocol>)object;
- (void)add:(id<JSDataProtocol>)object atIndex:(NSUInteger)index;
- (void)remove:(id<JSDataProtocol>)object;
- (void)removeAtIndex:(NSUInteger)index;
- (void)setValue:(NSMutableArray *)aArray;
- (NSMutableArray *)value;
- (NSUInteger)count;
- (id<JSDataProtocol>)first;
- (id<JSDataProtocol>)second;
- (id<JSDataProtocol>)rest;
- (id<JSDataProtocol>)last;
- (id<JSDataProtocol>)dropLast;
- (id<JSDataProtocol>)nth:(NSInteger)n;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (BOOL)isEmpty;
- (BOOL)isEqual:(JSList *)list;
- (NSUInteger)hash;
- (JSList *)reverse;
- (JSList *)drop:(NSInteger)n;
@end

NS_ASSUME_NONNULL_END
