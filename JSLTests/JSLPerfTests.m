//
//  JSLPerfTests.m
//  JSLTests
//
//  Created by jsloop on 23/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <JSL/JSLLib.h>

@interface JSLPerfTests : XCTestCase
@end

static NSString *form = @"(defun collatz (n) (collatz n [])) (defun collatz (n acc) (if (not= n 1) (if (even? n) (collatz (/ n 2) (conj acc n)) (collatz (+ (* 3 n) 1) (conj acc n))) (conj acc n)))";

@implementation JSLPerfTests {
    JSL *jsl;
}

- (void)setUp {
    jsl = [[JSL alloc] initWithoutREPL];
}

- (void)tearDown {
}

- (void)testCollatz {
    [jsl rep:form];
    XCTAssertEqualObjects([jsl rep:@"(count (collatz 93571393692802302))"], @"2092");
}

- (void)testCollatzPerformance {
    [jsl rep:form];
    [self measureBlock:^{
        [self->jsl rep:@"(collatz 93571393692802302)"];
    }];
}

- (void)testJSListDropFirstPerformance {
    NSMutableArray *arr = [NSMutableArray new];
    for (int i = 0; i < 10000; i++) {
        [arr addObject:@"1"];
    }
    JSList *xs = [[JSList alloc] initWithArray:arr];
    [self measureBlock:^{
        JSList *lst = (JSList *)[xs rest];
        for (int i = 0; i < 9999; i++) {
            lst = (JSList *)[lst rest];
        }
    }];
}

@end
