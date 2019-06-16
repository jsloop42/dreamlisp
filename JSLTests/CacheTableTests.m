//
//  CacheTableTests.m
//  JSLTests
//
//  Created by jsloop on 16/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "Types.h"
#import "CacheTable.h"

@interface CacheTableTests : XCTestCase

@end

@implementation CacheTableTests

- (void)setUp {
}

- (void)tearDown {
}

- (void)testCache {
    CacheTable *cache = [[CacheTable alloc] initWithSize:3];
    JSString *str1 = [[JSString alloc] initWithString:@"a"];
    JSString *str2 = [[JSString alloc] initWithString:@"b"];
    JSString *str3 = [[JSString alloc] initWithString:@"c"];
    JSString *str4 = [[JSString alloc] initWithString:@"d"];
    XCTAssertEqual([cache count], 0);
    [cache setObject:str1 forKey:str1];
    XCTAssertEqual([cache count], 1);
    XCTAssertEqualObjects([cache objectForKey:str1], str1);
    [cache setObject:str2 forKey:str2];
    [cache setObject:str3 forKey:str3];
    XCTAssertEqual([cache count], 3);
    [cache setObject:str4 forKey:str4];
    XCTAssertEqual([cache count], 3);
    XCTAssertNil([cache objectForKey:str1]);
    [cache clear];
    XCTAssertEqual([cache count], 0);
}

@end
