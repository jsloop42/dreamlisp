//
//  JSLTests.m
//  JSLTests
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <XCTest/XCTest.h>
#import "JSLTests.h"

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
    JSNumber* n1 = [[JSNumber alloc] initWithDouble:3.14];
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

}

- (void)testFileOps {
    XCTestExpectation *exp = [[XCTestExpectation alloc] initWithDescription:@"Appending and reading file content."];
    FileOps *fops = [[FileOps alloc] init];
    NSString *file = @"/tmp/fopstest.txt";
    NSString *content = @"foo\nbar";
    [fops createFileIfNotExist:file];
    XCTAssertNoThrow([fops append:content completion:^ {
        XCTAssertNoThrow([fops openFile:file]);
        XCTAssertTrue([fops hashNext]);
        NSString *text = @"";
        while ([fops hashNext]) {
            text = [text stringByAppendingString:[fops readLine]];
        }
        XCTAssertEqualObjects(text, @"foobar");
        [fops closeFile];
        XCTAssertTrue([fops delete:file]);
        [exp fulfill];
    }]);
    [self waitForExpectations:@[exp] timeout:10.0];
}

- (void)testHashMap {
    Reader *reader = [Reader new];
    Printer *printer = [Printer new];
    NSString *exp = @"{\"a\" \"abc\" \"b\" \"bcd\" \"c\" \"cde\"}";
    NSArray *inp = @[@"a", @"abc", @"b", @"bcd", @"c", @"cde"];
    JSData * data = [reader readString:exp];
    NSString *ret = [printer printStringFor:data readably:false];
    NSArray *arr = [[[ret stringByReplacingOccurrencesOfString:@"{" withString:@""] stringByReplacingOccurrencesOfString:@"}" withString:@""]
                    componentsSeparatedByString:@" "];
    XCTAssertEqual([arr count], [inp count]);
    for (int i = 0; i < [arr count]; i++) {
        XCTAssertTrue([inp containsObject:arr[i]]);
    }
}

- (void)testJSListRest {
    JSList *xs = [[JSList alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqual([xs count], 3);
    JSList *rest = (JSList *)[xs rest];
    XCTAssertEqual([xs count], 3);
    XCTAssertEqual([rest count], 2);
}

- (void)testJSListDropLast {
    JSList *xs = [[JSList alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqual([xs count], 3);
    JSList *list = (JSList *)[xs dropLast];
    XCTAssertEqual([xs count], 3);
    XCTAssertEqual([list count], 2);
    XCTAssertEqualObjects([list last], [xs second]);
}

- (void)testArithmeticFunctions {
    Core *core = [Core new];
    NSMutableDictionary *ns = [core namespace];
    JSData *(^add)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = [ns objectForKey:@"+"];
    JSData *(^sub)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = [ns objectForKey:@"-"];
    JSData *(^mul)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = [ns objectForKey:@"*"];
    JSData *(^div)(JSNumber *first, ...) NS_REQUIRES_NIL_TERMINATION = [ns objectForKey:@"/"];
    JSData *(^mod)(JSNumber *first, JSNumber *second) = [ns objectForKey:@"mod"];
    XCTAssertEqual([(JSNumber *)add([[JSNumber alloc] initWithString:@"1"], [[JSNumber alloc] initWithString:@"2"], [[JSNumber alloc] initWithString:@"3"],
                                    [[JSNumber alloc] initWithInt:4], [[JSNumber alloc] initWithInt:5], nil) intValue], 15);
    XCTAssertEqual([(JSNumber *)add([[JSNumber alloc] initWithDouble:1.5], [[JSNumber alloc] initWithDouble:2.0], [[JSNumber alloc] initWithDouble:3.0],
                                    [[JSNumber alloc] initWithDouble:4.0], [[JSNumber alloc] initWithDouble:5.5], nil) doubleValue], 16.0);
    XCTAssertEqual([(JSNumber *)sub([[JSNumber alloc] initWithDouble:5.5], [[JSNumber alloc] initWithDouble:1.0],
                                    [[JSNumber alloc] initWithDouble:2.0], nil) doubleValue], 2.5);
    XCTAssertEqual([(JSNumber *)mul([[JSNumber alloc] initWithDouble:1.5], [[JSNumber alloc] initWithDouble:2.5], [[JSNumber alloc] initWithDouble:3.5],
                                    [[JSNumber alloc] initWithDouble:4.0], [[JSNumber alloc] initWithDouble:5.5], nil) doubleValue], 288.75);
    XCTAssertEqualWithAccuracy([(JSNumber *)div([[JSNumber alloc] initWithDouble:125.5], [[JSNumber alloc] initWithDouble:25.0],
                                    [[JSNumber alloc] initWithDouble:2.0], [[JSNumber alloc] initWithDouble:2.0], nil) doubleValue], 1.255, 0.000000001);
    XCTAssertEqual([(JSNumber *)mod([[JSNumber alloc] initWithInt:5], [[JSNumber alloc] initWithInt:3]) intValue], 2);
}

-(void)testComparisonFunctions {
    Core *core = [Core new];
    NSMutableDictionary *ns = [core namespace];
    JSData *(^lessThan)(JSNumber *lhs, JSNumber *rhs) = [ns objectForKey:@"<"];
    JSData *(^greaterThan)(JSNumber *lhs, JSNumber *rhs) = [ns objectForKey:@">"];
    JSData *(^greaterThanOrEqual)(JSNumber *lhs, JSNumber *rhs) = [ns objectForKey:@">="];
    JSData *(^lessThanOrEqual)(JSNumber *lhs, JSNumber *rhs) = [ns objectForKey:@"<="];
    JSData *(^equalTo)(JSNumber *lhs, JSNumber *rhs) = [ns objectForKey:@"="];
    XCTAssertTrue([(JSBool *)lessThan([[JSNumber alloc] initWithInt:21], [[JSNumber alloc] initWithInt:42]) val]);
    XCTAssertFalse([(JSBool *)lessThan([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:21]) val]);
    XCTAssertTrue([(JSBool *)greaterThan([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:21]) val]);
    XCTAssertFalse([(JSBool *)greaterThan([[JSNumber alloc] initWithInt:21], [[JSNumber alloc] initWithInt:42]) val]);
    XCTAssertTrue([(JSBool *)greaterThanOrEqual([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:42]) val]);
    XCTAssertTrue([(JSBool *)greaterThanOrEqual([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:21]) val]);
    XCTAssertTrue([(JSBool *)lessThanOrEqual([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:42]) val]);
    XCTAssertTrue([(JSBool *)lessThanOrEqual([[JSNumber alloc] initWithInt:21], [[JSNumber alloc] initWithInt:42]) val]);
    XCTAssertTrue([(JSBool *)equalTo([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:42]) val]);
    XCTAssertFalse([(JSBool *)equalTo([[JSNumber alloc] initWithInt:42], [[JSNumber alloc] initWithInt:21]) val]);
}

- (void)notestPerformanceJSListDropFirst {
    NSMutableArray *arr = [NSMutableArray new];
    for (int i = 0; i < 10000; i++) {
        [arr addObject:@"1"];
    }
    JSList *xs = [[JSList alloc] initWithArray:arr];
    [self measureBlock:^{
        JSList *lst = (JSList *)[xs rest];
        for (int i = 0; i < 9999; i++) {
            lst = (JSList *)[lst rest];
        }
    }];
}

@end
