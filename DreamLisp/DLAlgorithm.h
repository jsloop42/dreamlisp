//
//  DLAlgorithm.h
//  DreamLisp
//
//  Created by jsloop on 07/07/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTypes.h"

NS_ASSUME_NONNULL_BEGIN

#define DLAlgo DLAlgorithm

@interface DLAlgorithm : NSObject
#pragma mark Sort
+ (void)introSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
+ (void)quickSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
+ (void)heapSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
+ (void)insertionSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending;
@end

NS_ASSUME_NONNULL_END
