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
#import "Terminal.h"

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

@end
