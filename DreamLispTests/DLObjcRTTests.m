//
//  DLObjcRTTests.m
//  DreamLispTests
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

@interface DLObjcRTTests : XCTestCase

@end

@implementation DLObjcRTTests

- (void)testSetterMethodMatch {
    NSString *setterName = @"setModuleName:";
    NSRegularExpression *pttn = [NSRegularExpression regularExpressionWithPattern:@"(set)(.*)(:)" options:0 error:nil];
    NSArray *matches = [DLUtils matchesInString:setterName withExpression:pttn];
    NSTextCheckingResult *match = nil;
    NSString *methodName = nil;
    for (match in matches) {
        if ([match numberOfRanges] == 4) {
            methodName = [setterName substringWithRange:[match rangeAtIndex:2]];
        }
    }
    NSString *effSetterName = [DLUtils toAccessorVar:methodName];
    XCTAssertEqualObjects(effSetterName, @"_moduleName");
}

- (void)testParseClassForm {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    NSString *dlCls = @"(defclass person1 (NSObject) ((name :initarg :with-name)))";
    DLObjc *rt = [DLObjc new];
    NSMutableArray *arr = [[dl reader] readString:dlCls];
    id<DLDataProtocol> ast = [arr first];
    DLClass *cls = [rt parseClassForm:ast];
    XCTAssertEqualObjects(cls.name.value, @"person1");
    XCTAssertEqual(cls.slots.count, 1);
    XCTAssertEqual(cls.conformance.count, 1);
    XCTAssertEqualObjects(cls.conformance.firstObject.value, @"NSObject");
    DLSlot *slot = cls.slots.firstObject;
    XCTAssertEqualObjects(slot.initializationArg.value, @":init-with-name");
    // Conformance parse error test
    dlCls = @"(defclass person2 ((name :initarg :with-name) (age :initarg :with-age)))";
    arr = [[dl reader] readString:dlCls];
    ast = [arr first];
    XCTAssertThrows([rt parseClassForm:ast]);
}

- (void)testCreateClass {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defclass bird (NSObject) ((wingspan :initarg :with-wing-span)))"];
    XCTAssertEqualObjects([dl rep:@"(type bird)"], @"\"class\"");
}

- (void)testMakeInstance {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defclass person (NSObject) ((name :initarg :with-name)))"];
    [dl rep:@"(def olive (make-instance 'person :init-with-name \"Olive\"))"];
    XCTAssertEqualObjects([dl rep:@"(type olive)"], @"\"object\"");
}

@end
