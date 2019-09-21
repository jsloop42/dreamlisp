//
//  DLAlgorithmTest.m
//  DreamLispTests
//
//  Created by jsloop on 07/07/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

@interface DLAlgorithmTest : XCTestCase
@end

@implementation DLAlgorithmTest

- (void)setUp {
}

- (void)tearDown {
}

- (void)testSort {
    NSMutableArray *arr = [@[@(2), @(10), @(7), @(3), @(6), @(8), @(11), @(18), @(15), @(13), @(12), @(4), @(9), @(5), @(16), @(14), @(20), @(19), @(1), @(17)]
                           mutableCopy];
    [arr sortUsingComparator:^NSComparisonResult(id  _Nonnull obj1, id  _Nonnull obj2) {
        if ([obj1 hash] < [obj2 hash]) {
            return NSOrderedAscending;
        }
        if ([obj1 hash] > [obj2 hash]) {
            return NSOrderedDescending;
        }
        return NSOrderedSame;
    }];
    XCTAssertEqualObjects(arr, ([@[@(1), @(2), @(3), @(4), @(5), @(6), @(7), @(8), @(9), @(10), @(11), @(12), @(13), @(14), @(15), @(16), @(17), @(18), @(19),
                                   @(20)] mutableCopy]));
}

#pragma mark Performance Tests

/*!
 Test sort
 Average: 0.563 seconds
 */
- (void)testSortUsingComparatorPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    NSInteger i = 0;
    for (i = 0; i < 10000; i++) {
        [arr addObject:@(arc4random_uniform(100000))];
    }
    [self measureBlock:^{
        [arr sortUsingComparator:^NSComparisonResult(id  _Nonnull obj1, id  _Nonnull obj2) {
            if ([obj1 hash] < [obj2 hash]) {
                return NSOrderedAscending;
            }
            if ([obj1 hash] > [obj2 hash]) {
                return NSOrderedDescending;
            }
            return NSOrderedSame;
        }];
    }];
}

@end
