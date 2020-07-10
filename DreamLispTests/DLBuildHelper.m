//
//  DLBuildHelper.m
//  DreamLispTests
//
//  Created by Jaseem V V on 22/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
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
    NSFileManager *fm = [[NSFileManager alloc] init];
    NSError *err;
    if ([_dbService checkIfPrefixStoreExists]) {
        NSURL *prefixStoreURL = [_dbService prefixStoreURL];
        NSURL *prefixStoreDir = [prefixStoreURL URLByDeletingLastPathComponent];
        NSURL *walFile = [prefixStoreDir URLByAppendingPathComponent:[[NSString alloc] initWithFormat:@"%@.sqlite-wal", DLConst.prefixStoreName]];
        NSURL *shmFile = [prefixStoreDir URLByAppendingPathComponent:[[NSString alloc] initWithFormat:@"%@.sqlite-shm", DLConst.prefixStoreName]];
        [fm removeItemAtURL:prefixStoreURL error:&err];
        XCTAssertNil(err);
        [fm removeItemAtURL:walFile error:&err];
        XCTAssertNil(err);
        [fm removeItemAtURL:shmFile error:&err];
        XCTAssertNil(err);
    }
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
    XCTAssertTrue([prefixList count] == 66);
    XCTAssertEqualObjects([prefixList firstObject], @"AK");
    XCTAssertEqualObjects([prefixList lastObject], @"XML");
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
                XCTAssertTrue(prefixes.count == 66);
                XCTAssertEqualObjects([[prefixes firstObject] name], @"AK");
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
                NSError *err;
                [_dbService.prefixStoreCoordinator removePersistentStore:_dbService.prefixStore error:&err];
                XCTAssertNil(err);
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
//    if ([fops isFileExists:dataPath]) {
//        [fops delete:dataPath];
//    }
    //XCTAssertFalse([fops isFileExists:dataPath]);
    [_dbService copyPrefixStoreToProject];
    XCTAssertTrue([fops isFileExists:dataPath]);
}

- (void)testI_initPersistence {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Init persistence expectation."];
    DLFileOps *fops = [DLFileOps new];
    NSURL *prefixStoreURL = [_dbService prefixStoreURL];
    NSString *prefixStorePath = [prefixStoreURL path];
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
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-string"], @"NSString");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-url-request"], @"NSURLRequest");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-ftp-url-handle"], @"NSFTPURLHandle");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-rest-url-handle"], @"NSRestURLHandle");  /* Not a foundation class + REST prefix is not added */
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-stream-socks-proxy-version4"], @"NSStreamSOCKSProxyVersion4");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-data-compression-algorithm-lzfse"], @"NSDataCompressionAlgorithmLZFSE");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-url-bookmark-resolution-without-ui"], @"NSURLBookmarkResolutionWithoutUI");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-xml-parser-cdata-not-finished-error"], @"NSXMLParserCDATANotFinishedError");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-metadata-item-resolution-width-dpi-key"], @"NSMetadataItemResolutionWidthDPIKey");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-script-command-description-more-i-vars"], @"NSScriptCommandDescriptionMoreIVars");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-xml-ns-number-transformer-name"], @"NSXMLNSNumberTransformerName");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-xml-sax-parser"], @"NSXMLSAXParser");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-ak-serializer-stream"], @"NSAKSerializerStream");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-layout-x-axis-anchor"], @"NSLayoutXAxisAnchor");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-is-linear-expression"], @"NSISLinearExpression");
    XCTAssertEqualObjects([DLUtils lispCaseToPascalCase:@"ns-cf-attributed-string"], @"NSCFAttributedString");
}

@end
