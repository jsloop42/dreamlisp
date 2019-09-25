//
//  DLBuildHelper.m
//  DreamLispTests
//
//  Created by jsloop on 22/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

static DLPersistenceService *_dbService;
static BOOL _isDBServiceInitialized;

@interface DLBuildHelper : XCTestCase
@end

/*!
 Build helper methods in the form of tests so that each of them can be run and verified. These are only for one off tasks that
 are less frequent. There are unit tests as well, which needs to be run serially in order. These relates to creation of the store, populating data, updating
 state, clearing data etc. It does not make much sense to have each test case do these steps independently.
 */
@implementation DLBuildHelper

- (void)setUp {
    if (!_isDBServiceInitialized) {
        _dbService = [DLPersistenceService new];
        _isDBServiceInitialized = YES;
    }
}

- (void)testA_PrefixStoreURL {
    NSURL *url = [_dbService prefixStoreURL];
    XCTAssertNotNil(url);
    XCTAssertTrue([[url lastPathComponent] isEqual:([[NSString alloc] initWithFormat:@"%@.sqlite", DLConst.prefixStoreName])]);
}

- (void)testB_InitPrefixStore {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Prefix Store Test Expectation"];
    NSURL *url = [_dbService prefixStoreURL];
    XCTAssertNotNil(url);
    NSFileManager *fm = [NSFileManager defaultManager];
    NSError *err;
    if ([_dbService checkIfPrefixStoreExists]) {
        [fm removeItemAtURL:url error:&err];
    }
    XCTAssertNil(err);
    XCTAssertFalse([_dbService checkIfPrefixStoreExists]);
    [_dbService initPrefixStore:^{
        XCTAssertTrue([_dbService checkIfPrefixStoreExists]);
        [exp fulfill];
    }];
    [self waitForExpectations:@[exp] timeout:5.0];
}

- (void)testC_LoadPrefixFromPList {
    NSArray *prefixList = [_dbService loadPrefixFromPList];
    XCTAssertNotNil(prefixList);
    XCTAssertTrue([prefixList count] == 61);
    XCTAssertEqualObjects([prefixList firstObject], @"ASCII");
    XCTAssertEqualObjects([prefixList lastObject], @"URL");
}

- (void)testD_InsertPrefixesToStoreInBatch {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Insert prefix expectation"];
    if (!_dbService.prefixContainer) {
        [_dbService initPrefixStore:^{
            XCTAssertNotNil(_dbService.prefixContainer);
            if (_dbService.prefixContainer) {
                [_dbService insertPrefixToStoreInBatch:^(BOOL status) {
                    XCTAssertTrue(status);
                    [exp fulfill];
                }];
            } else {
                [exp fulfill];
            }
        }];
    }
    [self waitForExpectations:@[exp] timeout:15.0];
}

- (void)testE_GetPrefixes {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Get prefix expectation"];
    if (!_dbService.prefixContainer) {
        [_dbService initPrefixStore:^{
            XCTAssertNotNil(_dbService.prefixContainer);
            if (_dbService.prefixContainer) {
                [_dbService getPrefixes:^(NSArray<DLPrefix *> *prefixes) {
                    XCTAssertNotNil(prefixes);
                    XCTAssertTrue(prefixes.count == 61);
                    XCTAssertEqualObjects([[prefixes firstObject] name], @"ASCII");
                    [exp fulfill];
                } isSort:YES];
            } else {
                [exp fulfill];
            }
        }];
    }
    [self waitForExpectations:@[exp] timeout:10.0];
}

- (void)testF_updateStateWithPrefix {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Get prefix expectation"];
    if (!_dbService.prefixContainer) {
        [_dbService initPrefixStore:^{
            XCTAssertNotNil(_dbService.prefixContainer);
            if (_dbService.prefixContainer) {
                [_dbService updateStateWithPrefix:^(BOOL status) {
                    XCTAssertTrue(status);
                    XCTAssertTrue(DLState.shared.prefixTree.children.count > 0);
                    [exp fulfill];
                }];
            } else {
                [exp fulfill];
            }
        }];
    }
    [self waitForExpectations:@[exp] timeout:10.0];
}

- (void)testG_prefixStoreProjectDataURL {
    NSURL *dataURL = [_dbService prefixStoreProjectDataURL];
    NSString *dataPath = [dataURL path];
    XCTAssertEqualObjects(dataPath, @"/Users/jsloop/dev/DreamLisp/data/DLPrefixModel.sqlite");
}

- (void)testH_copyPrefixToProject {
    NSURL *dataURL = [_dbService prefixStoreProjectDataURL];
    DLFileOps *fops = [DLFileOps new];
    NSString *dataPath = [dataURL path];
    if ([fops isFileExists:dataPath]) {
        [fops delete:dataPath];
    }
    XCTAssertFalse([fops isFileExists:dataPath]);
    [_dbService copyPrefixStoreToProject];
    XCTAssertTrue([fops isFileExists:dataPath]);
}

@end
