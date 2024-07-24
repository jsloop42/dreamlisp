//
//  NSMutableArray+DLList.h
//  DreamLisp
//
//  Created by Jaseem V V on 13/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSMutableArray (DLList)
+ (BOOL)isMutableArray:(id)object;
- (void)add:(id)object atIndex:(NSUInteger)index;
- (void)update:(id)object atIndex:(NSUInteger)index;
- (id)first;
- (id)second;
- (NSMutableArray *)rest;
- (id)last;
- (id)dropLast;
- (id)nth:(NSInteger)n;
- (BOOL)isEmpty;
- (NSMutableArray *)reverse;
- (NSMutableArray *)drop:(NSInteger)n;
- (id _Nullable)drop;
- (nonnull id)copyWithZone:(nullable NSZone *)zone;
- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone;
@end

NS_ASSUME_NONNULL_END
