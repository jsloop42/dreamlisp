//
//  DLObjcRTTests.m
//  DreamLispTests
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>

#define DL_EQUAL 0
#define DL_LESSTHAN -1
#define DL_GREATERTHAN 1

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
    NSString *effSetterName = [DLUtils toAccessorVarFromGetter:methodName];
    XCTAssertEqualObjects(effSetterName, @"_moduleName");
}

- (void)testUpdatePropertyAttribute {
    DLObjcPropertyAttr *attr = [DLObjcPropertyAttr new];
    [attr setValue:@"name"];
    [DLUtils updatePropertyAttr:attr];
    XCTAssertEqual(strncmp(attr.name, "name", strlen(attr.name)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.setterName, "setName:", strlen(attr.setterName)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.getterName, "name", strlen(attr.getterName)), DL_EQUAL);
}

- (void)testParseClasss {
    DLObjc *objc = [[DLObjc alloc] init];
    DLReader *reader = [[DLReader alloc] init];
    NSMutableArray *astxs = [reader readString:@"(defclass person (NSObject) ((name :initarg :with-name)))"];
    XCTAssertEqual(astxs.count, 1);
    id<DLDataProtocol> ast = [astxs first];
    DLClass *dlcls = [objc parseClassForm:ast];
    XCTAssertNotNil(dlcls);
    XCTAssertEqualObjects(dlcls.name.value, @"person");
    XCTAssertEqual(dlcls.conformance.count, 1);
    XCTAssertEqualObjects([(DLSymbol *)[dlcls.conformance first] value], @"NSObject");
    XCTAssertNil(dlcls.proxy);
    XCTAssertEqual(dlcls.slots.count, 1);
    DLSlot *slot = [dlcls.slots first];
    XCTAssertNotNil(slot.initializationArg);
    XCTAssertEqualObjects([slot.initializationArg string], @"init-with-name");
    XCTAssertNotNil(slot.value);
    XCTAssertEqualObjects(slot.value.value, @"name");
    const char *slotMethodType = [[[NSString alloc] initWithFormat:@"%s%s%s%s%s", @encode(id), @encode(id), @encode(SEL), @encode(id), @encode(id)] UTF8String];  // last arg is _cls
    XCTAssertEqual(strncmp(slot.methodType, slotMethodType, strlen(slotMethodType)), DL_EQUAL);
    XCTAssertEqual(slot.position, 0);
    XCTAssertNotNil(slot.attribute);
    DLObjcPropertyAttr *attr = slot.attribute;
    XCTAssertEqual(strncmp(attr.type, "@", strlen(attr.type)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.name, "name", strlen(attr.name)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.backingIvar, "_name", strlen(attr.backingIvar)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.type, "@", strlen(attr.type)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.getterName, "name", strlen(attr.getterName)), DL_EQUAL);
    XCTAssertEqual(strncmp(attr.setterName, "setName:", strlen(attr.setterName)), DL_EQUAL);
    XCTAssertFalse(attr.hasCustomGetter);
    XCTAssertFalse(attr.hasCustomSetter);
    XCTAssertFalse(attr.isReadOnly);
    XCTAssertFalse(attr.isCopy);
    XCTAssertFalse(attr.isRetain);
    XCTAssertFalse(attr.isNonAtomic);
    XCTAssertFalse(attr.isDynamic);
    XCTAssertFalse(attr.isWeakReference);
    XCTAssertFalse(attr.isEligibleForGC);
}

- (void)testDefClass {
    DLObjc *objc = [DLObjc new];
    DLReader *reader = [DLReader new];
    NSMutableArray *astList = [reader readString:@"(defclass persona (NSObject) ((universe :initarg :with-universe)))"];
    XCTAssertEqual([astList count], 1);
    id<DLDataProtocol> ast = [astList first];
    DLClass *dlCls = [objc parseClassForm:ast];
    XCTAssertNotNil(dlCls);
    XCTAssertNil(dlCls.proxy);
    XCTAssertNoThrow([objc defclass:dlCls]);
    XCTAssertNotNil(dlCls.proxy);
    XCTAssertTrue([objc isClassExists:dlCls.name.value]);
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

- (void)testDLOS {

}

@end
