//
//  JSLTests.m
//  JSLTests
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "JSLTests.h"

@interface JSLTests : XCTestCase

@end

@implementation JSLTests

- (void)setUp {

}

- (void)tearDown {

}

- (void)testDataType {
    JSString* str = [[JSString alloc] initWithString:@"Foo"];
    XCTAssertEqualObjects([str dataType], @"JSString");
}

- (void)testList {
    JSList* list = [JSList new];
    JSString* str = [[JSString alloc] initWithString:@"Foo"];
    JSString* str1 = str;
    [list add:str];
    [list add:str1];
    XCTAssertEqualObjects([(JSString*)[list first] value], @"Foo");
    XCTAssertEqual([list count], 2);
}

- (void)testNumber {
    JSNumber* n1 = [[JSNumber alloc] initWithFloat:3.14];
    JSNumber* n2 = n1;
    XCTAssertTrue([n1 isEqual:n2]);
}

- (void)testString {
    NSString *s1 = @"1";
    NSString *s2 = s1;
    s1 = @"2";
    XCTAssertEqualObjects(s2, @"1");
    JSString *s3 = [[JSString alloc] initWithFormat:@"%d", 42];
    XCTAssertEqualObjects([s3 value], @"42");
}

- (void)testTokenize {
    Reader *reader = [Reader new];
    NSString *exp = @"(+ 1 2)";
    NSArray *tokens = [reader tokenize:exp];
    NSMutableArray *tokensArr = [[NSMutableArray alloc] initWithObjects: @"(", @"+", @"1", @"2", @")", nil];
    XCTAssertEqualObjects(tokens, tokensArr);
}

- (void)testMapOnList {
    JSList *list = [[JSList alloc] initWithArray:@[@"abc", @"def", @"ghi"]];
    NSMutableArray *uc = [list map:^NSString * (NSString *arg) {
        return [arg uppercaseString];
    }];
    XCTAssertEqualObjects(uc[0], @"ABC");
}

- (void)testMapOnVector {
    JSVector *vec = [[JSVector alloc] initWithArray:@[@"abc", @"def", @"ghi"]];
    NSMutableArray *uc = [vec map:^NSString * (NSString *arg) {
        return [arg uppercaseString];
    }];
    XCTAssertEqualObjects(uc[0], @"ABC");
}


- (void)testMapOnDictionary {
    JSHashMap *hm = [[JSHashMap alloc] initWithArray:[@[@"a", @"abc", @"b", @"bcd", @"c", @"cde"] mutableCopy]];
    NSMutableArray *list = [hm map:^NSString *(NSString *key, NSString *obj) {
        return [obj uppercaseString];
    }];
    XCTAssertEqualObjects(list[0], @"ABC");
}

- (void)testPrintString {
    Printer *prn = [Printer new];
    // Function
    JSFunction *fn = [[JSFunction alloc] initWithAst:[JSNil new] params:[NSMutableArray new] env:[Env new] macro:false meta:[JSNil new]
                                                  fn:^id(id arg){return nil;}];
    XCTAssertEqualObjects([prn printStringFor:fn readably:true], @"#<function>");
    // Symbol
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@"greet"];
    XCTAssertEqualObjects([prn printStringFor:sym readably:true], @"greet");
    // Integer
    JSNumber *num = [[JSNumber alloc] initWithString:@"42"];
    XCTAssertEqual([num intValue], 42);
    XCTAssertEqualObjects([prn printStringFor:num readably:true], @"42");
    // Double
    JSNumber *num1 = [[JSNumber alloc] initWithString:@"42.42"];
    XCTAssertTrue([num1 isDouble]);
    XCTAssertEqual([num1 doubleValue], 42.42);
    XCTAssertEqualObjects([prn printStringFor:num1 readably:true], @"42.42");
    // List with numbers
    JSList *nlist = [[JSList alloc] initWithArray:@[@1, @2, @3]];
    XCTAssertEqualObjects([prn printStringFor:nlist readably:true], @"(1 2 3)");
    // List with strings
    JSList *slist = [[JSList alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqualObjects([prn printStringFor:slist readably:true], @"(\"1\" \"2\" \"3\")");
    // Keyword
    JSKeyword *kw = [[JSKeyword alloc] initWithKeyword:@"abc"];
    XCTAssertEqualObjects([prn printStringFor:kw readably:true], @":abc");
    // Vector with numbers
    JSVector *nvec = [[JSVector alloc] initWithArray:@[@1, @2, @3]];
    XCTAssertEqualObjects([prn printStringFor:nvec readably:true], @"[1 2 3]");
    // Vector with strings
    JSVector *svec = [[JSVector alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqualObjects([prn printStringFor:svec readably:true], @"[\"1\" \"2\" \"3\"]");
    // Hashmap [String: String]
    JSHashMap *hm1 = [[JSHashMap alloc] initWithArray:[@[@"a", @"abc", @"b", @"bcd", @"c", @"cde"] mutableCopy]];
    XCTAssertEqualObjects([prn printStringFor:hm1 readably:true], @"{\"a\" \"abc\" \"b\" \"bcd\" \"c\" \"cde\"}");
    JSHashMap *hm2 = [[JSHashMap alloc] initWithArray:[@[@"a", @123, @"b", @234, @"c", @345] mutableCopy]];
    XCTAssertEqualObjects([prn printStringFor:hm2 readably:true], @"{\"a\" 123 \"b\" 234 \"c\" 345}");
    JSHashMap *hm3 = [[JSHashMap alloc] initWithArray:[@[@"a", @[@123], @"b", @[@234], @"c", @[@345]] mutableCopy]];
    XCTAssertEqualObjects([prn printStringFor:hm3 readably:true], @"{\"a\" [123] \"b\" [234] \"c\" [345]}");
}

- (void)testReadPrint {
    Reader *reader = [Reader new];
    Printer *printer = [Printer new];
    NSString *(^print)(NSString *) = ^NSString *(NSString *exp) {
        return [printer printStringFor:[reader readString:exp] readably:true];
    };
    XCTAssertEqualObjects(print(@"(1 2 3)"), @"(1 2 3)");
    XCTAssertEqualObjects(print(@"[1 2 3]"), @"[1 2 3]");
    XCTAssertEqualObjects(print(@"(\"1\" \"2\" \"3\")"), @"(\"1\" \"2\" \"3\")");
    XCTAssertEqualObjects(print(@"{\"1\" \"2\"}"), @"{\"1\" \"2\"}");
    XCTAssertEqualObjects(print(@"{:1 \"2\"}"), @"{:1 \"2\"}");

}

- (void)notestPerformanceExample {
    // This is an example of a performance test case.
    [self measureBlock:^{
        // Put the code you want to measure the time of here.
    }];
}

@end
