//
//  DLVector.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLList.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLVector: DLList <DLDataProtocol>
+ (BOOL)isVector:(id)object;
+ (DLList *)dataToList:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLList *)dataToList:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta vector:(DLVector *)vector;
- (void)appendObject:(id<DLDataProtocol>)object;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block;
- (DLList *)list;
- (DLVector *)sort:(NSInteger (*)(id, id, void *))sorter;
- (DLVector *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
@end

NS_ASSUME_NONNULL_END
