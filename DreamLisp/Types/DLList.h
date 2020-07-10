//
//  DLList.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLError.h"
#import "DLTypeUtils.h"
#import "NSMutableArray+DLCat.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLList : NSObject <DLDataProtocol>
@property (nonatomic, readwrite, assign) NSInteger seekIndex;
+ (BOOL)isList:(id)object;
+ (BOOL)isKindOfList:(id)object;
+ (DLList *)dataToList:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLList *)dataToList:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta list:(DLList *)list;
- (id<DLDataProtocol> _Nullable)previous;
- (id<DLDataProtocol> _Nullable)next;
- (BOOL)hasNext;
- (void)add:(id<DLDataProtocol>)object;
- (void)add:(id<DLDataProtocol>)object atIndex:(NSUInteger)index;
- (DLList *)addObject:(id<DLDataProtocol>)object;
- (void)addObjectsFromArray:(NSMutableArray *)array;
- (void)addObjectsFromList:(DLList *)list;
- (void)update:(id<DLDataProtocol>)object atIndex:(NSUInteger)index;
- (void)remove:(id<DLDataProtocol>)object;
- (void)removeAtIndex:(NSUInteger)index;
- (NSUInteger)count;
- (id<DLDataProtocol>)first;
- (id<DLDataProtocol>)second;
- (id<DLDataProtocol>)rest;
- (id<DLDataProtocol> _Nullable)last;
- (id<DLDataProtocol>)dropLast;
- (id<DLDataProtocol>)nth:(NSInteger)n;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block;
- (BOOL)isEmpty;
- (DLList *)reverse;
- (DLList *)drop:(NSInteger)n;
- (DLList *)sort:(NSInteger (*)(id, id, void *))sorter;
- (DLList *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
- (NSMutableArray *)subArrayWithStartIndex:(NSInteger)start endIndex:(NSInteger)end;
@end

@interface DLList (NSFastEnumeration)

@end

NS_ASSUME_NONNULL_END
