//
//  DLCacheTableTests.m
//  DreamLispTests
//
//  Created by Jaseem V V on 16/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

@interface DLCacheTableTests : XCTestCase

@end

@implementation DLCacheTableTests

- (void)setUp {
}

- (void)tearDown {
}

- (void)testCache {
    DLCacheTable *cache = [[DLCacheTable alloc] initWithSize:3];
    DLString *str1 = [[DLString alloc] initWithString:@"a"];
    DLString *str2 = [[DLString alloc] initWithString:@"b"];
    DLString *str3 = [[DLString alloc] initWithString:@"c"];
    DLString *str4 = [[DLString alloc] initWithString:@"d"];
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
