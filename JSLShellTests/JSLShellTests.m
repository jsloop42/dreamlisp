//
//  JSLShellTests.m
//  JSLShellTests
//
//  Created by jsloop on 24/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "ShellConst.h"
#import "NSString+JSDataProtocol.h"
#import "MockStdIOService.h"
#import <JSL/JSLLib.h>

@interface JSLShellTests : XCTestCase

@end

@implementation JSLShellTests

- (void)setUp {
}

- (void)tearDown {
}

- (void)testVersion {
    NSString *ver = [ShellConst shellVersion];
    XCTAssertNotNil(ver);
    XCTAssertTrue([ver isNotEmpty]);
}

- (void)testIO {
    JSL *jsl = [[JSL alloc] init];
    MockStdIOService *stdIOService = [MockStdIOService new];
    [jsl setIsREPL:YES];
    [jsl bootstrap];
    [[jsl ioService] setStdIODelegate:stdIOService];
    [jsl printVersion];
    XCTAssertTrue([[stdIOService output] isNotEmpty]);
    [jsl loadCoreLib];
    // Test write output
    NSString *inp = @"(+ 1 2)";
    NSString *ret = nil;
    ret = [jsl rep:inp];
    [[jsl ioService] writeOutput:ret];
    XCTAssertEqualObjects([stdIOService output], @"3");
    // Test read input
    [stdIOService setInput:@"(* 21 2)"];
    ret = [jsl rep:[[jsl ioService] readInput]];
    [[jsl ioService] writeOutput:ret];
    XCTAssertEqualObjects([stdIOService output], @"42");
    [stdIOService setInput:@"(/ 21 2)"];
    ret = [jsl rep:@"(readline \"=> \")"];
    [[jsl ioService] writeOutput:ret];
    XCTAssertEqualObjects([stdIOService output], @"\"(/ 21 2)\"");
}

@end
