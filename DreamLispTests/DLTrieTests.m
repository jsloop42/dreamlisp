//
//  DLTrieTests.m
//  DreamLispTests
//
//  Created by jsloop on 21/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

@interface DLTrieTests : XCTestCase
@end

@implementation DLTrieTests

- (void)setUp {
}

- (void)tearDown {
}

- (void)testTrieInsert {
    NSString *str = @"NSString";
    DLTrie *trie = [DLTrie new];
    XCTAssertTrue([trie insert:str]);
    DLTrie *node = [trie findNodeWithName:@"N"];
    XCTAssertNotNil(node);
}

- (void)testTrieSearch {
    DLTrie *trie = [DLTrie new];
    XCTAssertTrue([trie insert:@"NS"]);
    XCTAssertTrue([trie insert:@"NSURL"]);
    XCTAssertTrue([trie insert:@"NSURLSession"]);
    DLTrieSearchResult *result = [trie search:@"NSURL" isResultInCaps:YES];
    XCTAssertTrue(result.isExist);
    XCTAssertEqual(result.prefixes.count, 2);
    /* near match */
    result = [trie search:@"NSURLConnection" isResultInCaps:YES];
    XCTAssertFalse(result.isExist);
    XCTAssertEqual(result.prefixes.count, 2);
}

- (void)testTrieDelete {
    DLTrie *trie = [DLTrie new];
    XCTAssertTrue([trie insert:@"NS"]);
    XCTAssertTrue([trie insert:@"NSURL"]);
    XCTAssertTrue([trie insert:@"NSURLSession"]);
    DLTrieSearchResult *result = [trie search:@"NSURLSession" isResultInCaps:YES];
    DLRet ret = [trie delete:@"NSURLSession"];
    XCTAssertEqual(ret, DLRetOK);
    result = [trie search:@"NSURLSession" isResultInCaps:YES];
    XCTAssertFalse(result.isExist);
}

- (void)notestPerformanceExample {
    [self measureBlock:^{
    }];
}

@end
