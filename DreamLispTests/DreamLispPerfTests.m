//
//  DreamLispPerfTests.m
//  DreamLispTests
//
//  Created by jsloop on 23/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

@interface DLPerfTests : XCTestCase
@end

static NSString *form = @"(defun collatz (n) (collatz n [])) (defun collatz (n acc) (if (not= n 1) (if (even? n) (collatz (/ n 2) (conj acc n)) (collatz (+ (* 3 n) 1) (conj acc n))) (conj acc n)))";

@implementation DLPerfTests {
    DreamLisp *dl;
}

- (void)setUp {
    dl = [[DreamLisp alloc] initWithoutREPL];
}

- (void)tearDown {
}

- (void)testCollatz {
    [dl rep:form];
    XCTAssertEqualObjects([dl rep:@"(count (collatz 93571393692802302))"], @"2092");
}

- (void)testCollatzPerformance {
    [dl rep:form];
    [self measureBlock:^{
        [self->dl rep:@"(collatz 93571393692802302)"];
    }];
}

- (void)testDLListDropFirstPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    for (int i = 0; i < 10000; i++) {
        [arr addObject:@"1"];
    }
    DLList *xs = [[DLList alloc] initWithArray:arr];
    [self measureBlock:^{
        DLList *lst = (DLList *)[xs rest];
        for (int i = 0; i < 9999; i++) {
            lst = (DLList *)[lst rest];
        }
    }];
}

@end
