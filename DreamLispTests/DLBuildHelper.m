//
//  DLBuildHelper.m
//  DreamLispTests
//
//  Created by jsloop on 22/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

@interface DLBuildHelper : XCTestCase

@end

/*!
 Build helper methods in the form of tests so that each of them can be run and verified. These are only for one off tasks that
 are less frequent.
 */
@implementation DLBuildHelper

/*!
 Method to generate prefix serialized file from the prefix plist.
 */
- (void)testSerializePrefixFile {
    XCTAssertTrue([DLUtils generatePrefixState]);
    XCTAssertTrue([DLUtils initializePrefixState]);
    DLFileOps *fops = [DLFileOps new];
    NSString *binFilePath = [[NSString alloc] initWithFormat:@"%@%@", fops.projectRoot, DLConst.prefixBinFilePathFrag];
    XCTAssertTrue([fops isFileExists:binFilePath]);
    fops.path = binFilePath;
    NSLog(@"file size: %ld", [fops fileSize]);
    XCTAssertTrue([fops fileSize] > 26100);
}

@end
