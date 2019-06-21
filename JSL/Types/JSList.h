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
+ (JSList *)dataToList:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSList *)dataToList:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta list:(JSList *)list;
- (void)add:(id<JSDataProtocol>)object;
- (void)add:(id<JSDataProtocol>)object atIndex:(NSUInteger)index;
- (JSList *)addObject:(id<JSDataProtocol>)object;
- (void)addObjectsFromArray:(NSMutableArray *)array;
- (void)addObjectsFromList:(JSList *)list;
- (void)update:(id<JSDataProtocol>)object atIndex:(NSUInteger)index;
- (void)remove:(id<JSDataProtocol>)object;
- (void)removeAtIndex:(NSUInteger)index;
- (NSUInteger)count;
- (id<JSDataProtocol>)first;
- (id<JSDataProtocol>)second;
- (id<JSDataProtocol>)rest;
- (id<JSDataProtocol> _Nullable)last;
- (id<JSDataProtocol>)dropLast;
- (id<JSDataProtocol>)nth:(NSInteger)n;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block;
- (BOOL)isEmpty;
- (JSList *)reverse;
- (JSList *)drop:(NSInteger)n;
- (JSList *)sort:(NSInteger (*)(id, id, void *))sorter;
- (JSList *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
- (NSMutableArray *)subArrayWithStartIndex:(NSInteger)start endIndex:(NSInteger)end;
@end

NS_ASSUME_NONNULL_END
