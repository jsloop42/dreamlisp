//
//  Algorithm.h
//  JSL
//
//  Created by jsloop on 07/07/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"

NS_ASSUME_NONNULL_BEGIN

#define Algo Algorithm

@interface Algorithm : NSObject
#pragma mark Sort
+ (void)introSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
+ (void)quickSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
+ (void)heapSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
+ (void)insertionSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
@end

NS_ASSUME_NONNULL_END
