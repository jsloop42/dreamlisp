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

- (void)notestCreateClass {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defclass bird (NSObject) ((wingspan :initarg :with-wing-span)))"];
    XCTAssertEqualObjects([dl rep:@"(type bird)"], @"\"class\"");
}

- (void)notestMakeInstance {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defclass person (NSObject) ((name :initarg :with-name)))"];
    [dl rep:@"(def olive (make-instance 'person :init-with-name \"Olive\"))"];
    XCTAssertEqualObjects([dl rep:@"(type olive)"], @"\"object\"");
}

- (void)testCamelCaseToLispCase {
    XCTAssertEqualObjects([DLUtils camelCaseToLispCase:@"componentsSeparatedByString"], @"components-separated-by-string");
    XCTAssertEqualObjects([DLUtils camelCaseToLispCase:@"componentsSeparatedByNSString"], @"components-separated-by-ns-string");
    XCTAssertEqualObjects([DLUtils camelCaseToLispCase:@"componentsSeparatedByDLString"], @"components-separated-by-dl-string");
    XCTAssertEqualObjects([DLUtils camelCaseToLispCase:@"componentsSeparatedByStringDL"], @"components-separated-by-string-dl");
    XCTAssertEqualObjects([DLUtils camelCaseToLispCase:@"componentsseparatedbystring"], @"componentsseparatedbystring");
}

- (void)testSelectorStringConversion {
    /* Test selector string convertion from camel case to lisp case */
    DLMethod *method = [DLMethod new];
    method.name = [[DLSymbol alloc] initWithName:@"genRandom"];
    DLMethodParam *param = [DLMethodParam new];
    param.selectorName = [[DLKeyword alloc] initWithString:@"separatedByNum"];
    [method.params addObject:param];
    param = [DLMethodParam new];
    param.selectorName = [[DLKeyword alloc] initWithString:@"withDLRange"];
    [method.params addObject:param];
    [DLUtils updateSelectorStringForMethod:method];
    XCTAssertEqualObjects(method.selectorString.value, @"gen-random:separated-by-num:with-dl-range:");
}

@end
