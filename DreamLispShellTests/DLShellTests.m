//
//  DLShellTests.m
//  DreamLispShellTests
//
//  Created by jsloop on 24/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "DLShellConst.h"
#import "NSString+DLCat.h"
#import "DLMockStdIOService.h"
#import <DreamLisp/DreamLispLib.h>

@interface DLShellTests : XCTestCase

@end

@implementation DLShellTests

- (void)setUp {
}

- (void)tearDown {
}

- (void)testVersion {
    NSString *ver = [DLShellConst shellVersion];
    XCTAssertNotNil(ver);
    XCTAssertTrue([ver isNotEmpty]);
}

- (void)testIO {
    DreamLisp *dl = [[DreamLisp alloc] init];
    DLMockStdIOService *stdIOService = [DLMockStdIOService new];
    [dl setIsREPL:YES];
    [dl bootstrap];
    [[dl ioService] setStdIODelegate:stdIOService];
    [dl printVersion];
    XCTAssertTrue([[stdIOService output] isNotEmpty]);
    [dl loadDLModuleLibs];
    // Test write output
    NSString *inp = @"(+ 1 2)";
    NSString *ret = nil;
    ret = [dl rep:inp];
    [[dl ioService] writeOutput:ret];
    XCTAssertEqualObjects([stdIOService output], @"3");
    // Test read input
    [stdIOService setInput:@"(* 21 2)"];
    ret = [dl rep:[[dl ioService] readInput]];
    [[dl ioService] writeOutput:ret];
    XCTAssertEqualObjects([stdIOService output], @"42");
    [stdIOService setInput:@"(/ 21 2)"];
    ret = [dl rep:@"(readline \"=> \")"];
    [[dl ioService] writeOutput:ret];
    XCTAssertEqualObjects([stdIOService output], @"\"(/ 21 2)\"");
}

@end
