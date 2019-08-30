//
//  Algorithm.m
//  DreamLisp
//
//  Created by jsloop on 07/07/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "Algorithm.h"

@implementation Algorithm

#pragma mark Sort

+ (void)introSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending {
    NSInteger len = [xs count];
    NSInteger maxDepth = 2 * floor(log2(len));
    [self introSort:xs lowerBound:0 upperBound:len - 1 depthLimit:maxDepth isAscending:isAscending];
}

+ (void)introSort:(NSMutableArray *)xs lowerBound:(NSInteger)lowerBound upperBound:(NSInteger)upperBound depthLimit:(NSInteger)depthLimit
      isAscending:(BOOL)isAscending {
    if (upperBound - lowerBound > 16) {
        if (depthLimit == 0) {
            [self heapSort:xs isAscending:isAscending];
            return;
        }
        --depthLimit;
        NSInteger pivot = [self findPivot:xs x:lowerBound y:lowerBound + (((upperBound - lowerBound) / 2) + 1) z:upperBound];
        [xs exchangeObjectAtIndex:pivot withObjectAtIndex:upperBound];
        NSInteger p = [self partition:xs lowerBound:lowerBound upperBound:upperBound isAscending:isAscending];
        [self introSort:xs lowerBound:lowerBound upperBound:p - 1 depthLimit:depthLimit isAscending:isAscending];
        [self introSort:xs lowerBound:p + 1 upperBound:upperBound depthLimit:depthLimit isAscending:isAscending];
    } else {
        [self insertionSort:xs isAscending:isAscending];
    }
}

+ (void)quickSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending {
    [self quickSort:xs lowerBound:0 upperBound:[xs count] - 1 isAscending:isAscending];
}

+ (void)quickSort:(NSMutableArray *)xs lowerBound:(NSInteger)lowerBound upperBound:(NSInteger)upperBound isAscending:(BOOL)isAscending {
    if (lowerBound < upperBound) {
        NSInteger p = [self partition:xs lowerBound:lowerBound upperBound:upperBound isAscending:isAscending];
        [self quickSort:xs lowerBound:lowerBound upperBound:p - 1 isAscending:isAscending];
        [self quickSort:xs lowerBound:p + 1 upperBound:upperBound isAscending:isAscending];
    }
}

+ (void)heapSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending {
    NSInteger n = [xs count];
    NSInteger i;
    for (i = floor(n / 2.0) - 1; i >= 0; i--) {
        [self reheap:xs upperBound:n index:i isAscending:isAscending];
    }
    for (i = n - 1; i >= 0; i--) {
        [xs exchangeObjectAtIndex:0 withObjectAtIndex:i];
        [self reheap:xs upperBound:i index:0 isAscending:isAscending];
    }
}

+ (void)reheap:(NSMutableArray *)xs upperBound:(NSInteger)n index:(NSInteger)i isAscending:(BOOL)isAscending {
    NSInteger pivot = i;
    NSInteger left = 2 * i + 1;
    NSInteger right = 2 * i + 2;
    if (left < n && ((isAscending && [xs[left] sortValue] > [xs[pivot] sortValue]) || (!isAscending && [xs[left] sortValue] < [xs[pivot] sortValue]))) {
        pivot = left;
    }
    if (right < n && ((isAscending && [xs[right] sortValue] > [xs[pivot] sortValue]) || (!isAscending && [xs[right] sortValue] < [xs[pivot] sortValue]))) {
        pivot = right;
    }
    if (pivot != i) {
        [xs exchangeObjectAtIndex:i withObjectAtIndex:pivot];
        [self reheap:xs upperBound:n index:pivot isAscending:isAscending];
    }
}

+ (NSInteger)findPivot:(NSMutableArray *)xs x:(NSInteger)x y:(NSInteger)y z:(NSInteger)z {
    NSInteger max = MAX(MAX([xs[x] sortValue], [xs[y] sortValue]), [xs[z] sortValue]);
    NSInteger min = MIN(MIN([xs[x] sortValue], [xs[y] sortValue]), [xs[z] sortValue]);
    NSInteger median = max ^ min ^ [xs[x] sortValue] ^ [xs[y] sortValue] ^ [xs[z] sortValue];
    if (median == [xs[x] sortValue]) {
        return x;
    }
    if (median == [xs[y] sortValue]) {
        return y;
    }
    return z;
}

+ (NSInteger)partition:(NSMutableArray *)xs lowerBound:(NSInteger)lowerBound upperBound:(NSInteger)upperBound isAscending:(BOOL)isAscending {
    id pivot = xs[upperBound];
    NSInteger i = lowerBound - 1;
    NSInteger j;
    for (j = lowerBound; j < upperBound; j++) {
        if ([xs[j] sortValue] <= [pivot sortValue]) {
            i++;
            [xs exchangeObjectAtIndex:i withObjectAtIndex:j];
        }
    }
    [xs exchangeObjectAtIndex:i + 1 withObjectAtIndex:upperBound];
    return i + 1;
}

+ (void)insertionSort:(NSMutableArray *)xs isAscending:(BOOL)isAscending {
    NSInteger len = [xs count];
    NSInteger i = 1;
    NSInteger j = 0;
    id elem = nil;
    while (i < len) {
        elem = xs[i];
        j = i - 1;
        while (j >= 0 && ((isAscending && [xs[j] sortValue] > [elem sortValue]) || (!isAscending && [xs[j] sortValue] < [elem sortValue]))) {
            xs[j + 1] = xs[j];
            j = j - 1;
        }
        xs[j + 1] = elem;
        i = i + 1;
    }
}

@end
