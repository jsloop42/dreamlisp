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
static DLIOService *_ioService;
static DLLogger *_logger;

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
        [DLLogger setIsDebug:YES];
        _ioService = [DLIOService new];
        _logger = [DLLogger new];
        _ioService.stdIODelegate = _logger;
        [DLLogger setIOService:_ioService];
    }
}

- (void)testA_PrefixStoreURL {
    NSURL *url = [_dbService prefixStoreURL];
    XCTAssertNotNil(url);
    XCTAssertTrue([[url lastPathComponent] isEqual:([[NSString alloc] initWithFormat:@"%@.sqlite", DLConst.prefixStoreName])]);
}

- (void)testB_InitPrefixStore {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Prefix Store Test Expectation"];
    BOOL ret = [_dbService deletePrefixStore];
    XCTAssertTrue(ret);
    XCTAssertFalse([_dbService checkIfPrefixStoreExists]);
    [_dbService initPrefixStore:^{
        XCTAssertTrue([_dbService isPrefixStoreInitialized]);
        XCTAssertTrue([_dbService checkIfPrefixStoreExists]);
        [exp fulfill];
    }];
    [self waitForExpectations:@[exp] timeout:10.0];
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
    void (^process)(void) = ^void(void) {
        XCTAssertTrue([_dbService isPrefixStoreInitialized]);
        XCTAssertNotNil(_dbService.prefixContainer);
        if (_dbService.prefixContainer) {
            [_dbService insertPrefixToStoreInBatch:^(BOOL status) {
                XCTAssertTrue(status);
                [exp fulfill];
            }];
        } else {
            [exp fulfill];
        }
    };
    if (!_dbService.prefixContainer) {
        [_dbService initPrefixStore:^{
            process();
        }];
    } else {
        process();
    }
    [self waitForExpectations:@[exp] timeout:30.0];
}

- (void)testE_GetPrefixes {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Get prefix expectation"];
    void (^process)(void) = ^void(void) {
        XCTAssertTrue([_dbService isPrefixStoreInitialized]);
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
    };
    if (!_dbService.prefixContainer) {
        [_dbService initPrefixStore:^{
            process();
        }];
    } else {
        process();
    }
    [self waitForExpectations:@[exp] timeout:30.0];
}

- (void)testF_updateStateWithPrefix {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Get prefix expectation"];
    void (^process)(void) = ^void(void) {
        XCTAssertTrue([_dbService isPrefixStoreInitialized]);
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
    };
    if (!_dbService.prefixContainer) {
        [_dbService initPrefixStore:^{
            process();
        }];
    } else {
        process();
    }
    [self waitForExpectations:@[exp] timeout:30.0];
}

- (void)testG_prefixStoreProjectDataURL {
    NSURL *dataURL = [_dbService prefixStoreProjectDataURL];
    NSString *dataPath = [dataURL path];
    XCTAssertEqualObjects(dataPath, @"/Users/jsloop/dev/DreamLisp/Data/DLPrefixModel.sqlite");
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

- (void)testI_initPersistence {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Init persistence expectation."];
    DLFileOps *fops = [DLFileOps new];
    NSURL *prefixStoreURL = [_dbService prefixStoreURL];
    NSString *prefixStorePath = [prefixStoreURL path];
    XCTAssertTrue([_dbService deletePrefixStore]);
    XCTAssertFalse([fops isFileExists:prefixStorePath]);
    [_dbService initPersistence:^(BOOL status) {
        XCTAssertTrue(status);
        XCTAssertTrue([_dbService isPrefixStoreInitialized]);
        XCTAssertTrue([fops isFileExists:prefixStorePath]);
        XCTAssertTrue(DLState.shared.prefixTree.children.count > 1);
        [exp fulfill];
    }];
    [self waitForExpectations:@[exp] timeout:40.0];
}

- (void)testJ_lispCaseToPascalCase {
    XCTAssertTrue(DLState.shared.prefixTree.children.count > 1);
}

@end
