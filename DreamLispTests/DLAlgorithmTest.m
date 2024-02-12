//
//  DLAlgorithmTest.m
//  DreamLispTests
//
//  Created by Jaseem V V on 07/07/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
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

- (void)testInsertionSort {
    NSMutableArray *arr = [@[@(2), @(10), @(7), @(3), @(6), @(8), @(11), @(18), @(15), @(13), @(12), @(4), @(9), @(5), @(16), @(14), @(20), @(19), @(1), @(17)]
                           mutableCopy];
    [DLAlgo insertionSort:arr isAscending:YES];
    XCTAssertEqualObjects(arr, ([@[@(1), @(2), @(3), @(4), @(5), @(6), @(7), @(8), @(9), @(10), @(11), @(12), @(13), @(14), @(15), @(16), @(17), @(18), @(19),
                                   @(20)] mutableCopy]));
//    arr = [@[@2, @3, @6, @1, @7, @5] mutableCopy];
//    [DLAlgo insertionSort:arr isAscending:YES];
//    XCTAssertEqualObjects(arr, ([@[@1, @2, @3, @5, @6, @7] mutableCopy]));
//    arr = [@[@2, @3, @6, @1, @7, @4, @5] mutableCopy];
//    [DLAlgo insertionSort:arr isAscending:NO];
//    XCTAssertEqualObjects(arr, ([@[@7, @6, @5, @4, @3, @2, @1] mutableCopy]));
}

- (void)testHeapSort {
    NSMutableArray *arr = [@[@(2), @(10), @(7), @(3), @(6), @(8), @(11), @(18), @(15), @(13), @(12), @(4), @(9), @(5), @(16), @(14), @(20), @(19), @(1), @(17)]
                           mutableCopy];
    [DLAlgo heapSort:arr isAscending:YES];
    XCTAssertEqualObjects(arr, ([@[@(1), @(2), @(3), @(4), @(5), @(6), @(7), @(8), @(9), @(10), @(11), @(12), @(13), @(14), @(15), @(16), @(17), @(18), @(19),
                                   @(20)] mutableCopy]));
    arr = [@[@2, @3, @6, @1, @7, @4, @5] mutableCopy];
    [DLAlgo insertionSort:arr isAscending:NO];
    XCTAssertEqualObjects(arr, ([@[@7, @6, @5, @4, @3, @2, @1] mutableCopy]));
}

- (void)testIntroSort {
    NSMutableArray *arr = [@[@(2), @(10), @(7), @(3), @(6), @(8), @(11), @(18), @(15), @(13), @(12), @(4), @(9), @(5), @(16), @(14), @(20), @(19), @(1), @(17)]
                           mutableCopy];
    [DLAlgo introSort:arr isAscending:YES];
    XCTAssertEqualObjects(arr, ([@[@(1), @(2), @(3), @(4), @(5), @(6), @(7), @(8), @(9), @(10), @(11), @(12), @(13), @(14), @(15), @(16), @(17), @(18), @(19),
                                   @(20)] mutableCopy]));
    arr = [@[@(2), @(10), @(7), @(3), @(6), @(8), @(11), @(18), @(15), @(13), @(12), @(4), @(9), @(5), @(16), @(14), @(20), @(19), @(1), @(17)]
           mutableCopy];
    [DLAlgo introSort:arr isAscending:NO];
    XCTAssertEqualObjects(arr, ([@[@20, @19, @18, @17, @16, @15, @14, @13, @12, @11, @10, @9, @8, @7, @6, @5, @4, @3, @2, @1] mutableCopy]));
    arr = [@[@2, @3, @6, @1, @7, @4, @5] mutableCopy];
    [DLAlgo introSort:arr isAscending:NO];
    XCTAssertEqualObjects(arr, ([@[@7, @6, @5, @4, @3, @2, @1] mutableCopy]));
}

- (void)testQuickSort {
    NSMutableArray *arr = [@[@(2), @(10), @(7), @(3), @(6), @(8), @(11), @(18), @(15), @(13), @(12), @(4), @(9), @(5), @(16), @(14), @(20), @(19), @(1), @(17)]
                           mutableCopy];
    [DLAlgo quickSort:arr isAscending:YES];
    XCTAssertEqualObjects(arr, ([@[@(1), @(2), @(3), @(4), @(5), @(6), @(7), @(8), @(9), @(10), @(11), @(12), @(13), @(14), @(15), @(16), @(17), @(18), @(19),
                                   @(20)] mutableCopy]));
}

- (void)testInsertionSortPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    NSInteger i = 0;
    for (i = 0; i < 10000; i++) {
        [arr addObject:@(arc4random_uniform(100000))];
    }
    [self measureBlock:^{
        [DLAlgo insertionSort:arr isAscending:YES];
    }];
}

- (void)testHeapSortPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    NSInteger i = 0;
    for (i = 0; i < 10000; i++) {
        [arr addObject:@(arc4random_uniform(100000))];
    }
    [self measureBlock:^{
        [DLAlgo heapSort:arr isAscending:YES];
    }];
}

- (void)testQuickSortPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    NSInteger i = 0;
    for (i = 0; i < 10000; i++) {
        [arr addObject:@(arc4random_uniform(100000))];
    }
    [self measureBlock:^{
        [DLAlgo quickSort:arr isAscending:YES];
    }];
}

- (void)testIntroSortPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    NSInteger i = 0;
    for (i = 0; i < 10000; i++) {
        [arr addObject:@(arc4random_uniform(100000))];
    }
    [self measureBlock:^{
        [DLAlgo introSort:arr isAscending:YES];
    }];
}
@end
