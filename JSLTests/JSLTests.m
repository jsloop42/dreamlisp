//
//  JSLTests.m
//  JSLTests
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <Foundation/Foundation.h>
#import <XCTest/XCTest.h>
#import <JSL/JSLLib.h>
#import "MockStdIOService.h"

@interface JSLTests : XCTestCase
@end

@implementation JSLTests

- (void)setUp {

}

- (void)tearDown {

}

- (void)testNSMapTable {
    NSMapTable *table1 = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    [table1 setObject:@"1" forKey:@"1"];
    NSMapTable *table2 = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    [table2 setObject:@"1" forKey:@"1"];
    XCTAssertTrue([table1 isEqual:table2]);
}

- (void)testNSMutableArray {
    NSMutableArray *arr = [@[@1, @2, @3, @4] mutableCopy];
    XCTAssertEqualObjects([arr drop:0], arr);
    NSMutableArray *res = [@[@2, @3, @4] mutableCopy];
    XCTAssertEqualObjects([arr drop:1], res);
    res = [@[@3, @4] mutableCopy];
    XCTAssertEqualObjects([arr drop:2], res);
    res = [@[@1, @2, @3] mutableCopy];
    XCTAssertEqualObjects([arr drop:-1], res);
    res = [@[@1, @2] mutableCopy];
    XCTAssertEqualObjects([arr drop:-2], res);
    res = [@[@1] mutableCopy];
    XCTAssertEqualObjects([arr drop:-3], res);
    res = [@[] mutableCopy];
    XCTAssertEqualObjects([arr drop:-4], res);
}

- (void)testDataType {
    JSString* str = [[JSString alloc] initWithString:@"Foo"];
    XCTAssertEqualObjects([str dataType], @"JSString");
}

- (void)testJSList {
    JSList* list = [JSList new];
    JSString* str = [[JSString alloc] initWithString:@"Foo"];
    JSString* str1 = str;
    [list add:str];
    [list add:str1];
    XCTAssertEqualObjects([(JSString*)[list first] value], @"Foo");
    XCTAssertEqual([list count], 2);
    list = [[JSList alloc] initWithArray: [@[[[JSString alloc] initWithString:@"1"], [[JSString alloc] initWithString:@"2"],
                                             [[JSString alloc] initWithString:@"3"], [[JSString alloc] initWithString:@"4"]] mutableCopy]];

    XCTAssertEqualObjects([(JSString *)[[list reverse] first] value], @"4");
    XCTAssertEqualObjects([(JSString *)[list first] value], @"1");
}

- (void)testJSNumber {
    JSNumber* n1 = [[JSNumber alloc] initWithDouble:3.14];
    JSNumber* n2 = n1;
    XCTAssertTrue([n1 isEqual:n2]);
}

- (void)testJSString {
    NSString *s1 = @"1";
    NSString *s2 = s1;
    s1 = @"2";
    XCTAssertEqualObjects(s2, @"1");
    JSString *s3 = [[JSString alloc] initWithFormat:@"%d", 42];
    XCTAssertFalse([s3 isMutable]);
    XCTAssertEqualObjects([s3 value], @"42");
    // Mutable test
    NSMutableString *str = [NSMutableString new];
    [str appendString:@"abc"];
    JSString *mstr = [[JSString alloc] initWithMutableString:str];
    XCTAssertTrue([mstr isMutable]);
    [mstr appendString:@"d"];
    XCTAssertEqualObjects([mstr mutableValue], @"abcd");
    // reverse
    XCTAssertEqualObjects([[[JSString alloc] initWithString:@"abcd"] reverse], @"dcba");
    XCTAssertEqualObjects([[[JSString alloc] initWithString:@"abcdefg"] reverse], @"gfedcba");
    XCTAssertEqualObjects([[[JSString alloc] initWithString:@"a"] reverse], @"a");
    XCTAssertEqualObjects([[[JSString alloc] initWithString:@""] reverse], @"");
    XCTAssertEqualObjects([[[JSString alloc] initWithString:@"1234"] reverse], @"4321");
    XCTAssertEqualObjects([[[JSString alloc] initWithString:@"[1 2 3 4]"] reverse], @"]4 3 2 1[");
    // joined
    JSString *str1 = [[JSString alloc] initWithString:@"a"];
    JSString *str2 = [[JSString alloc] initWithString:@"b"];
    JSString *str3 = [[JSString alloc] initWithString:@"c"];
    JSString *str4 = [[JSString alloc] initWithMutableString];
    NSArray<JSString *> *arr = @[str1, str2, str3];
    XCTAssertEqualObjects([[str4 joined:arr with:@", "] value], @"a, b, c");
}

- (void)testJSStringSubString {
    JSString *str = [[JSString alloc] initWithString:@"abcdefg"];
    // test substringFrom:to
    XCTAssertEqualObjects([str substringFrom:0 to:0], @"a");
    XCTAssertEqualObjects([str substringFrom:0 to:1], @"ab");
    XCTAssertEqualObjects([str substringFrom:1 to:1], @"b");
    XCTAssertEqualObjects([str substringFrom:1 to:2], @"bc");
    XCTAssertEqualObjects([str substringFrom:1 to:6], @"bcdefg");
    XCTAssertThrows([str substringFrom:1 to:7]);
    XCTAssertThrows([str substringFrom:1 to:0]);
    // test substringFrom:count
    XCTAssertEqualObjects([str substringFrom:0 count:0], @"");
    XCTAssertEqualObjects([str substringFrom:0 count:1], @"a");
    XCTAssertEqualObjects([str substringFrom:0 count:2], @"ab");
    XCTAssertEqualObjects([str substringFrom:1 count:2], @"bc");
    XCTAssertEqualObjects([str substringFrom:4 count:3], @"efg");
    XCTAssertThrows([str substringFrom:5 count:4]);
    XCTAssertThrows([str substringFrom:0 count:-1]);
    // test substringFrom
    XCTAssertEqualObjects([str substringFrom:3], @"defg");
    XCTAssertEqualObjects([str substringFrom:6], @"g");
    XCTAssertEqualObjects([str substringFrom:8], @"");
    XCTAssertEqualObjects([str substringFrom:10], @"");
}

- (void)testJSKeyword {
    JSKeyword *kwd = [[JSKeyword alloc] initWithString:@"foo"];
    XCTAssertEqualObjects([kwd value], @":foo");
    kwd = [[JSKeyword alloc] initWithKeyword:@":foo"];
    XCTAssertEqualObjects([kwd value], @":foo");
    kwd = [[JSKeyword alloc] initWithKeyword:@"foo"];
    XCTAssertEqualObjects([kwd string], @"foo");
    kwd = [[JSKeyword alloc] initWithKeyword:@":abc"];
    XCTAssertEqualObjects([kwd encoded], @"\u029e:abc");
    kwd = [[JSKeyword alloc] initWithEncodedKeyword:@"\u029e:abc"];
    XCTAssertEqualObjects([kwd value], @":abc");
    XCTAssertFalse([JSKeyword isKeyword:[[JSNumber alloc] initWithInt:1]]);
    XCTAssertFalse([JSKeyword isKeyword:[JSString new]]);
}

- (void)testJSSymbol {
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@"count" moduleName:[Const coreModuleName]]; // `(core:count a [1])
    [sym setArity:-2];
    [sym resetArity];
    [sym setModuleName:[Const defaultModuleName]];
    [sym setPosition:0];
    [sym setIsQualified:YES];
    XCTAssertEqualObjects([sym string], @"core:count");
}

- (void)testJSSymbolComparison {
    JSSymbol *sym1 = [[JSSymbol alloc] initWithName:@"a" moduleName:[State currentModuleName]];
    [sym1 setArity:-2];
    JSSymbol *sym2 = [[JSSymbol alloc] initWithName:@"b" moduleName:[State currentModuleName]];
    [sym2 setArity:-1];
    JSSymbol *sym3 = [[JSSymbol alloc] initWithName:@"c" moduleName:[State currentModuleName]];
    [sym3 setArity:0];
    JSSymbol *sym4 = [[JSSymbol alloc] initWithName:@"d" moduleName:[State currentModuleName]];
    [sym4 setArity:1];
    NSMutableArray *arr = [@[sym3, sym4, sym2, sym1] mutableCopy];
    [arr sortUsingComparator:^NSComparisonResult(id  _Nonnull obj1, id  _Nonnull obj2) {
        return [JSSymbol compareSymbol:obj1 withSymbol:obj2];
    }];
    XCTAssertEqualObjects([arr first], sym2);
    XCTAssertEqualObjects([arr second], sym4);
    XCTAssertEqualObjects([arr last], sym1);
}

- (void)testJSSymbolProcess {
    [State setCurrentModuleName:[Const defaultModuleName]];
    JSSymbol *sym = [JSSymbol processName:@"mod:func/1"];
    XCTAssertEqualObjects([sym moduleName], [State currentModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], @"mod");
    XCTAssertEqual([sym arity], 1);
    XCTAssertEqual([sym initialArity], 1);
    XCTAssertTrue([sym isQualified]);
    XCTAssertTrue([sym isFunction]);
    sym = [JSSymbol processName:@"func/1"];
    XCTAssertEqualObjects([sym moduleName], [State currentModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], [State currentModuleName]);
    XCTAssertEqual([sym arity], 1);
    XCTAssertEqual([sym initialArity], 1);
    XCTAssertFalse([sym isQualified]);
    XCTAssertTrue([sym isFunction]);
    sym = [JSSymbol processName:@"var"];
    XCTAssertEqualObjects([sym moduleName], [State currentModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], [State currentModuleName]);
    XCTAssertEqual([sym arity], -2);
    XCTAssertEqual([sym initialArity], -2);
    XCTAssertFalse([sym isQualified]);
    XCTAssertFalse([sym isFunction]);
}

- (void)testTokenize {
    Reader *reader = [Reader new];
    NSString *exp = @"(+ 1 2)";
    NSArray *tokens = [reader tokenize:exp];
    NSMutableArray *tokensArr = [[NSMutableArray alloc] initWithObjects: @"(", @"+", @"1", @"2", @")", nil];
    XCTAssertEqualObjects(tokens, tokensArr);
}

- (void)testMapOnJSList {
    JSList *list = [[JSList alloc] initWithArray:@[@"abc", @"def", @"ghi"]];
    NSMutableArray *uc = [list map:^NSString * (NSString *arg) {
        return [arg uppercaseString];
    }];
    XCTAssertEqualObjects(uc[0], @"ABC");
}

- (void)testMapOnVJSector {
    JSVector *vec = [[JSVector alloc] initWithArray:@[@"abc", @"def", @"ghi"]];
    NSMutableArray *uc = [vec map:^NSString * (NSString *arg) {
        return [arg uppercaseString];
    }];
    XCTAssertEqualObjects(uc[0], @"ABC");
}

- (void)testReader {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    Reader *reader = [jsl reader];
    NSMutableArray<id<JSDataProtocol>> *ast = [reader readString:@"(def a 1)"];
    JSList *xs = (JSList *)ast[0];
    XCTAssertEqualObjects([(JSSymbol *)[xs first] moduleName], [Const defaultModuleName]);
    XCTAssertEqualObjects([(JSSymbol *)[xs second] moduleName], [Const defaultModuleName]);
    XCTAssertEqualObjects([reader moduleName], [Const defaultModuleName]);
    ast = [reader readString:@"(defmodule foo (export all))"];
    XCTAssertEqualObjects([reader moduleName], @"foo");
    ast = [reader readString:@"(def a (fn (n) (+ n 1)))"];
    xs = (JSList *)ast[0];
    XCTAssertEqualObjects([(JSSymbol *)[xs second] moduleName], @"foo");
    xs = [xs nth:2];
    XCTAssertEqualObjects([(JSSymbol *)[(JSList *)[xs nth:1] first] moduleName], @"foo");
    xs = [xs nth:2];
    XCTAssertEqualObjects([(JSSymbol *)[xs nth:1] moduleName], @"foo");
    ast = [reader readString:@"(in-module \"user\")"];
    XCTAssertEqualObjects([reader moduleName], @"foo");
    [jsl rep:@"(in-module \"user\")"];
    XCTAssertEqualObjects([reader moduleName], [Const defaultModuleName]);
    ast = [reader readString:@"(def b 2)"];
    xs = (JSList *)ast[0];
    XCTAssertEqualObjects([(JSSymbol *)[xs second] moduleName], [Const defaultModuleName]);
    ast = [reader readString:@"(core:empty? [1])"];
    xs = (JSList *)ast[0];
    JSSymbol *sym = (JSSymbol *)[xs first];
    XCTAssertEqualObjects([sym moduleName], [Const defaultModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], [Const coreModuleName]);
    XCTAssertTrue([sym isQualified]);
}

- (void)testPrintString {
    Printer *prn = [Printer new];
    // Function
    JSFunction *fn = [[JSFunction alloc] initWithAst:[JSNil new] params:[NSMutableArray new]
                      env:[Env new] macro:false meta:[JSNil new] fn:^id(id arg) { return nil; } name:@"nil-fn/0"];
    XCTAssertEqualObjects([prn printStringFor:fn readably:true], @"nil-fn/0");
    // Symbol
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@"greet"];
    [sym setModuleName:[Const defaultModuleName]];
    XCTAssertEqualObjects([prn printStringFor:sym readably:true], @"user:greet");
    // Integer
    JSNumber *num = [[JSNumber alloc] initWithString:@"42"];
    XCTAssertEqual([num integerValue], 42);
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
    JSKeyword *kw = [[JSKeyword alloc] initWithKeyword:@":abc"];
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
        return [printer printStringFor:[reader readString:exp][0] readably:true];
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
        XCTAssertTrue([fops hasNext]);
        NSString *text = @"";
        while ([fops hasNext]) {
            text = [text stringByAppendingString:[fops readLine]];
        }
        XCTAssertEqualObjects(text, @"foobar");
        [fops closeFile];
        XCTAssertTrue([fops delete:file]);
        [exp fulfill];
    }]);
    [self waitForExpectations:@[exp] timeout:10.0];
}

- (void)testJSHashMap {
    Reader *reader = [Reader new];
    Printer *printer = [Printer new];
    NSString *exp = @"{\"a\" \"abc\" \"b\" \"bcd\" \"c\" \"cde\"}";
    NSArray *inp = @[@"a", @"abc", @"b", @"bcd", @"c", @"cde"];
    id<JSDataProtocol> data = [reader readString:exp][0];
    NSString *ret = [printer printStringFor:data readably:false];
    NSArray *arr = [[[ret stringByReplacingOccurrencesOfString:@"{" withString:@""] stringByReplacingOccurrencesOfString:@"}" withString:@""]
                    componentsSeparatedByString:@" "];
    XCTAssertEqual([arr count], [inp count]);
    for (int i = 0; i < [arr count]; i++) {
        XCTAssertTrue([inp containsObject:arr[i]]);
    }
    JSHashMap *dict = [JSHashMap new];
    NSString *key = @"sym";
    JSString *object = [[JSString alloc] initWithString:@"1234"];
    [dict setObject:object forKey:key];
    XCTAssertEqualObjects((JSString *)[dict objectForKey:key], object);
    // testing hash map with object keys (should implement to `isEqual`, `hash` functions).
    NSMutableDictionary *aDict = [NSMutableDictionary new];
    JSNumber *aVal = [[JSNumber alloc] initWithInt:1];
    JSNumber *aKey = [[JSNumber alloc] initWithInt:2];
    [aDict setObject:aVal forKey:aKey];
    id<JSDataProtocol> aRet = [aDict objectForKey:aKey];
    NSArray *aKeys = [aDict allKeys];
    XCTAssertTrue([aKeys count] == 1);
    aRet = [aDict objectForKey:aKeys[0]];
    XCTAssertNotNil(aRet);
    XCTAssertEqualObjects([aRet dataType], @"JSNumber");
    aRet = nil;
    aKeys = nil;
    // testing hash map with number keys
    JSHashMap *hm = [[JSHashMap alloc] initWithArray:[@[aKey, aVal] mutableCopy]];
    aKeys = [hm allKeys];
    XCTAssertTrue([aKeys count] == 1);
    aRet = [hm objectForKey:aKeys[0]];
    XCTAssertNotNil(aRet);
    XCTAssertEqualObjects([aRet dataType], @"JSNumber");
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

- (void)testLoadingCoreLib {
    JSL *jsl = [[JSL alloc] init];
    [jsl bootstrap];
    [jsl loadCoreLib];
    XCTAssertTrue([[Env modules] containsKey:[Const coreModuleName]]);
}

- (void)testSymbol {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(symbol \"foo:bar/1\")"], @"foo:bar/1");
    XCTAssertEqualObjects([jsl rep:@"(def z 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(eval (symbol \"user:z\"))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"1\")"], @"*:1");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"a\")"], @"*:a");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"abc\")"], @"*:abc");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"abc/1\")"], @"*:abc/1");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"abc/n\")"], @"*:abc/n");
    // Test qualified symbol
    XCTAssertEqualObjects([jsl rep:@"(symbol \"foo:1\")"], @"foo:1");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"foo:a\")"], @"foo:a");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"foo:abc\")"], @"foo:abc");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"foo:abc/1\")"], @"foo:abc/1");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"foo:abc/n\")"], @"foo:abc/n");
    // function argument
    [jsl rep:@"(def a core:inc/1)"];
    XCTAssertEqualObjects([jsl rep:@"(symbol a)"], @"user:a/1");
    XCTAssertEqualObjects([jsl rep:@"(symbol (fn (n) 1))"], @"*:*/1");
}

- (void)testArithmeticEval {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(+ 1 2)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(+ 1 2 3)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(+ 1 2 3 4)"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(+ 1 2 3 -4)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(- 1 2)"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(- 5 1 1)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(- 5 3 2 1)"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(- 5 4 -2 1)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(* 5 4 -2 1)"], @"-40");
    XCTAssertEqualObjects([jsl rep:@"(- (- 5 4) 2)"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(+ (- (- 5 4) 2) 43)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(* (- (- 5 4) 2) -42)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(/ (+ (* (* 5 -4) -4) 4) 2)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(/ (+ (* (* 5.5 -4.20) -4.41) 4.314) 2.24)"], @"47.404017857142857142857142857142857142");
    XCTAssertEqualObjects([jsl rep:@"(+ 5 (* 2 3))"], @"11");
    XCTAssertEqualObjects([jsl rep:@"(- (+ 5 (* 2 3)) 3)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(/ (- (+ 5 (* 2 3)) 3) 4)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(/ (- (+ 515 (* 87 311)) 302) 27)"], @"1010");
    XCTAssertEqualObjects([jsl rep:@"(* -3 6)"], @"-18");
    XCTAssertEqualObjects([jsl rep:@"(/ (- (+ 515 (* -87 311)) 296) 27)"], @"-994");
    XCTAssertEqualObjects([jsl rep:@"[1 2 (+ 1 2)]"], @"[1 2 3]");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" (+ 7 8)}"], @"{\"a\" 15}");
    XCTAssertEqualObjects([jsl rep:@"{:a (+ 7 8)}"], @"{:a 15}");
    XCTAssertEqualObjects([jsl rep:@"(/ 100 2 2)"], @"25");
    XCTAssertEqualObjects([jsl rep:@"(/ 100 2 2 2)"], @"12.5");
    XCTAssertEqualObjects([jsl rep:@"(/ 100 2 2 2 2)"], @"6.25");
    XCTAssertEqualObjects([jsl rep:@"(/ 100 2 2 2 2 2)"], @"3.125");
    XCTAssertEqualObjects([jsl rep:@"(/ 100 2 2 2 2 2 2)"], @"1.5625");
    XCTAssertEqualObjects([jsl rep:@"(/ (- (+ 5 (* 2 3)) 3) 4)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(mod 42 21)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(mod 5 3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(mod 5 3.5)"], @"1.5");
    XCTAssertEqualObjects([jsl rep:@"(mod 5 -3.5)"], @"-2.0");
    // zero argument
    XCTAssertEqualObjects([jsl rep:@"(+)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(*)"], @"1");
    // one argument
    XCTAssertEqualObjects([jsl rep:@"(+ 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(+ 5)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(+ 7.0)"], @"7.0");
    XCTAssertEqualObjects([jsl rep:@"(+ 31.1)"], @"31.1");
    XCTAssertEqualObjects([jsl rep:@"(- 1)"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(- 1.0)"], @"-1.0");
    XCTAssertEqualObjects([jsl rep:@"(- 5.0)"], @"-5.0");
    XCTAssertEqualObjects([jsl rep:@"(- 53.6)"], @"-53.6");
    XCTAssertEqualObjects([jsl rep:@"(* 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(* 3.1415)"], @"3.1415");
    XCTAssertEqualObjects([jsl rep:@"(/ 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(/ -1)"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(/ 1.0)"], @"1.0");
    XCTAssertEqualObjects([jsl rep:@"(/ 2)"], @"0.5");
    XCTAssertEqualObjects([jsl rep:@"(/ -2)"], @"-0.5");
    XCTAssertEqualObjects([jsl rep:@"(/ 4)"], @"0.25");
}

- (void)testComparisonFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(< 42 84)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(> 42 21)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 42 42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(> 42 43)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(>= 42 43)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(<= 42 41)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 42 42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 42 21)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 42.0 42.0)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 42.42 42.42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 42.42 42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(> 42.42 42.21)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(< 42.42 42.21)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(<= 42.42 42.21)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(< 42.21 42.42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(<= 42.42 42.42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 42.42 42.42)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(> 47.404017857142857142857142857142857142 47.404017857142857142857142857142857141)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 47.404017857142857142857142857142857142 47.404017857142857142857142857142857142)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(> 2 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(> 1 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(> 1 2)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(>= 2 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 1 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 1 2)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(< 2 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(< 1 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(< 1 2)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(<= 2 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(<= 1 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(<= 1 2)"], @"true");
    // testing one argument
    XCTAssertEqualObjects([jsl rep:@"(<= 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(<= 0)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 1.1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(>= 1)"], @"true");
}

- (void)testPrintFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    MockStdIOService *stdIOService = [MockStdIOService new];
    [[jsl ioService] setStdIODelegate:stdIOService];
    XCTAssertEqualObjects([jsl rep:@"(println [33 2 3])"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"[33 2 3]");
    XCTAssertEqualObjects([jsl rep:@"(prn [(+ 21 12) 2 3])"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"[33 2 3]");
    XCTAssertEqualObjects([jsl rep:@"(prn)"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"");
    XCTAssertEqualObjects([jsl rep:@"(prn \"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc  def\" \"ghi jkl\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc  def\" \"ghi jkl\"");
    XCTAssertEqualObjects([jsl rep:@"(prn \"\\\"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc\\ndef\\nghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc\\ndef\\nghi\"");
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc\\\\\\\\def\\\\\\\\ghi\"");
    XCTAssertEqualObjects([jsl rep:@"(prn (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"(1 2 \"abc\" \"\\\"\") \"def\"");
    // (println)
    XCTAssertEqualObjects([jsl rep:@"(println)"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"");
    XCTAssertEqualObjects([jsl rep:@"(println \"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"");
    XCTAssertEqualObjects([jsl rep:@"(println \"abc\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc");
    XCTAssertEqualObjects([jsl rep:@"(println \"abc  def\" \"ghi jkl\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc  def ghi jkl");
    XCTAssertEqualObjects([jsl rep:@"(println \"\\\"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"");
    XCTAssertEqualObjects([jsl rep:@"(println \"abc\\ndef\\nghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc\ndef\nghi");
    XCTAssertEqualObjects([jsl rep:@"(println \"abc\\\\def\\\\ghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc\\def\\ghi");
    XCTAssertEqualObjects([jsl rep:@"(println (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"(1 2 abc \") def");
}

- (void)testDef {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // def with quote in symbol name
    XCTAssertEqualObjects([jsl rep:@"(def x 10)"], @"10");
    XCTAssertEqualObjects([jsl rep:@"x"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(def a' 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"a'"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def b' '(11 12 13 14))"], @"(11 12 13 14)");
    XCTAssertEqualObjects([jsl rep:@"b'"], @"(11 12 13 14)");
}

- (void)testList {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"()"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(list)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(list 1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(list 1 (list 21 22 23) 3)"], @"(1 (21 22 23) 3)");
    XCTAssertEqualObjects([jsl rep:@"(first nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(rest nil)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(first [])"], @"nil");
}

- (void)testVector {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(if [] 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(count [1 2 3])"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(empty? [1 2 3])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(empty? [])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(list? [4 5 6])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [] (list))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [7 8] [7 8])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1 2) [1 2])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1) [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [] [1])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 0 [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [] 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [] \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"\" [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"((fn [] 4) )"], @"4");
    XCTAssertEqualObjects([jsl rep:@"((fn [f x] (f x)) (fn [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(= [(list)] (list []))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(vector 3 4 5)"], @"[3 4 5]");
    XCTAssertEqualObjects([jsl rep:@"[:a :b :c]"], @"[:a :b :c]");
    XCTAssertEqualObjects([jsl rep:@"[:a (+ 1 2) \"z\"]"], @"[:a 3 \"z\"]");
}

- (void)testHashMap {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"abc\" 1}"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" (+ 1 2)}"], @"{\"a\" 3}");
    XCTAssertEqualObjects([jsl rep:@"{:a (+ 7 8)}"], @"{:a 15}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc {:a 1 :b 2} :a)"], @"{:b 2}");
    NSString *ret = [jsl rep:@"(keys {:abc 123 :def 456})"];
    XCTAssertTrue([ret isEqual:@"(:abc :def)"] || [ret isEqual:@"(:def :abc)"]);
    XCTAssertEqualObjects([jsl rep:@"(contains? :abc {:abc nil})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? :abc {:abc 123})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(get :abc {:abc 123})"], @"123");
    [jsl rep:@"(def hm4 (assoc {:a 1 :b 2} :a 3 :c 1))"];
    XCTAssertEqualObjects([jsl rep:@"(get :a hm4)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(get :b hm4)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(get :c hm4)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(hash-map \"a\" 1)"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} \"a\" 1)"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(get \"a\" (assoc (assoc {\"a\" 1 } \"b\" 2) \"c\" 3))"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def hm1 (hash-map))"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? hm1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? \"abc\")"], @"false");
    XCTAssertThrows([jsl rep:@"(get \"a\" nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(get \"a\" hm1)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"a\" hm1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(def hm2 (assoc hm1 \"a\" 1))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(get \"a\" hm1)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"a\" hm1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(get \"a\" hm2)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"a\" hm2)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keys hm1)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(keys hm2)"], @"(\"a\")");
    XCTAssertEqualObjects([jsl rep:@"(values hm1)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(values hm2)"], @"(1)");
    XCTAssertEqualObjects([jsl rep:@"(count (keys (assoc hm2 \"b\" 2 \"c\" 3)))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} :bcd 234)"], @"{:bcd 234}");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth 0 (keys {:abc 123 :def 456})))"], @"true");
    [jsl rep:@"(def hm3 (assoc hm2 \"b\" 2))"];
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth 0 (keys {\":abc\" 123 \":def\" 456})))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth 0 (values {\"a\" :abc \"b\" :def})))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} :bcd nil)"], @"{:bcd nil}");
    // overwrite duplicate key
    XCTAssertEqualObjects([jsl rep:@"(assoc {:a 1} :a 3)"], @"{:a 3}");
    // testing dissoc
    [jsl rep:@"(def hm3 (assoc hm2 \"b\" 2))"];
    XCTAssertEqualObjects([jsl rep:@"(count (keys hm3))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(count (values hm3))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(dissoc hm3 \"a\")"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc hm3 \"a\" \"b\")"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc hm3 \"a\" \"b\" \"c\")"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(count (keys hm3))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(count hm3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(count {:a 1 :b 2 :c 3 :d 4})"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(dissoc {:cde 345 :fgh 456} :cde)"], @"{:fgh 456}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc {:cde nil :fgh 456} :cde)"], @"{:fgh 456}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc {:a 1 :b 4} :a)"], @"{:b 4}");
    // testing equality
    XCTAssertEqualObjects([jsl rep:@"(= {} {})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 11 :b 22} (hash-map :b 22 :a 11))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 11 :b [22 33]} (hash-map :b [22 33] :a 11))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 11 :b {:c 33}} (hash-map :b {:c 33} :a 11))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 11 :b 22} (hash-map :b 23 :a 11))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 11 :b 22} (hash-map :a 11))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= {:a [11 22]} {:a (list 11 22)})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 11 :b 22} (list :a 11 :b 22))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= {} [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [] {})"], @"false");
    // null key
    XCTAssertEqualObjects([jsl rep:@"(def a nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} a 2)"], @"{nil 2}");
    // any key
    XCTAssertEqualObjects([jsl rep:@"{1 1}"], @"{1 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{[\"x\" \"y\"] 1}"], @"{[\"x\" \"y\"] 1}");
    // hash map key evaluation
    XCTAssertEqualObjects([jsl rep:@"(def a 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"{a 2}"], @"{1 2}");
    XCTAssertThrows([jsl rep:@"{b 2}"], @"'b' not found");
    XCTAssertEqualObjects([jsl rep:@"(contains? a {a 2})"], @"true");
}

/** Test hash map keyword key as get function*/
- (void)testHashMapKeywordFuncKey {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(:a {:a '(+ 1 2)})"], @"(user:+ 1 2)");
    [jsl rep:@"(def khm {:a 2 :b 3 :c 5 :d [10 11 \"a\"]})"];
    XCTAssertEqualObjects([jsl rep:@"(:a khm)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(:b khm)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(:c khm)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(:d khm)"], @"[10 11 \"a\"]");
    XCTAssertEqualObjects([jsl rep:@"(:a {:a 1})"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(:a (hash-map :a 1))"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def tag :success)"], @":success");
    XCTAssertEqualObjects([jsl rep:@"(def hm (atom {:success 0}))"], @"(atom {:success 0})");
    XCTAssertEqualObjects([jsl rep:@"(reset! hm (assoc @hm tag (+ (get tag @hm) 1)))"], @"{:success 1}");
}

- (void)testEnv {
    Env *env = [Env new];
    [env setModuleName:[Const defaultModuleName]];
    JSString *obj = [[JSString alloc] initWithString:@"123"];
    JSSymbol *key = [[JSSymbol alloc] initWithName:@"key"];
    [key setModuleName:[Const defaultModuleName]];
    [env setObject:obj forKey:key];
    XCTAssertEqualObjects([env objectForKey:key], obj);
    Env *aEnv = [[Env alloc] initWithEnv:env];  // Nested env
    JSSymbol *aKey = [[JSSymbol alloc] initWithName:@"aKey"];
    JSString *aObj = [[JSString alloc] initWithString:@"987"];
    [aKey setModuleName:[Const defaultModuleName]];
    [aEnv setObject:aObj forKey:aKey];
    XCTAssertEqualObjects([aEnv objectForKey:aKey], aObj);
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(list? *ARGV*)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"*ARGV*"], @"()");
    // Test with reader
    Reader *reader = [Reader new];  // In default module, user
    Env *denv = [[Env alloc] initWithModuleName:[Const defaultModuleName] isUserDefined:NO];
    NSMutableArray *xs = [reader readString:@"(def a 1)"];
    JSList *list = (JSList *)[xs first];
    key = [list nth:1];
    id<JSDataProtocol> elem = [list nth:2];
    [denv setObject:elem forKey:key];
    XCTAssertEqual([[denv exportTable] count], 1);
    XCTAssertEqual([[denv exportTable] objectForSymbol:key], elem);
    [reader readString:@"(defmodule foo (export all))"];
    xs = [reader readString:@"(def b 2)"];
    list = (JSList *)[xs first];
    key = [list nth:1];
    elem = [list nth:2];
    Env *fooEnv = [[Env alloc] initWithModuleName:@"foo" isUserDefined:YES];
    [fooEnv setObject:elem forKey:key];
    XCTAssertEqual([[fooEnv exportTable] count], 0);
    XCTAssertEqual([[fooEnv internalTable] count], 1);
    XCTAssertEqual([[fooEnv internalTable] objectForSymbol:key], elem);
}

- (void)testSpecialForms {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // def
    XCTAssertEqualObjects([jsl rep:@"(def x 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"x"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(def x 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"x"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(def y (+ 1 7))"], @"8");
    XCTAssertEqualObjects([jsl rep:@"y"], @"8");
    // case sensitive symbols
    XCTAssertEqualObjects([jsl rep:@"(def mynum 111)"], @"111");
    XCTAssertEqualObjects([jsl rep:@"(def MYNUM 222)"], @"222");
    XCTAssertEqualObjects([jsl rep:@"mynum"], @"111");
    XCTAssertEqualObjects([jsl rep:@"MYNUM"], @"222");
    // env lookup error
    XCTAssertEqualObjects([jsl rep:@"(try (abc 1 2 3) (catch ex (str ex)))"], @"\"'user:abc/3' not found\"");
    // error aborts def being re-set
    XCTAssertEqualObjects([jsl rep:@"(def w 123)"], @"123");
    XCTAssertThrows([jsl rep:@"(def w (abc))"], @"Symbol not found");
    XCTAssertEqualObjects([jsl rep:@"w"], @"123");
    // let form
    XCTAssertEqualObjects([jsl rep:@"(let (z (+ 2 3)) (+ 1 z))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(let [z 9] z)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(let (x 9) x)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"x"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(let (z (+ 2 3)) (+ 1 z))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(let (p (+ 2 3) q (+ 2 p)) (+ p q))"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def y (let (z 7) z))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"y"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(let (x (or nil \"yes\")) x)"], @"\"yes\"");
    // outer env
    XCTAssertEqualObjects([jsl rep:@"(def a 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(let (q 9) q)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(let (q 9) a)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(let (z 2) (let (q 9) a))"], @"4");
    // let with vector binding
    XCTAssertEqualObjects([jsl rep:@"(let [z 9] z)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(let [p (+ 2 3) q (+ 2 p)] (+ p q))"], @"12");
    // vector evaluation
    XCTAssertEqualObjects([jsl rep:@"(let (a 5 b 6) [3 4 a [b 7] 8])"], @"[3 4 5 [6 7] 8]");
}

/** Test core forms with do */
- (void)testCoreForms {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // Test fn
    [jsl rep:@"(def a (atom 0))"];
    XCTAssertEqualObjects([jsl rep:@"(def sum (fn (n) (reset! a n) (reset! a (+ @a 1))))"], @"user:sum/1");
    XCTAssertEqualObjects([jsl rep:@"(sum 5)"], @"6");
    // Test let
    XCTAssertEqualObjects([jsl rep:@"(let (x 1 y 2) (reset! a x) (reset! a (+ x y)))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(let (a 11 b (fn (n) (+ n 1)) c (atom 0)) (reset! c (+ a 10)) (b @c))"], @"22");
    XCTAssertEqualObjects([jsl rep:@"(try (reset! a 10) (reset! a (+ @a 2)))"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(try (throw \"foo\") (catch e (reset! a 1) (reset! a (+ @a 10))))"], @"11");
}

- (void)testFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"((fn (a b) (+ b a)) 3 4)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"((fn [f x] (f x)) (fn [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(((fn (a) (fn (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"((fn (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"((fn () 4))"], @"4");
    XCTAssertEqualObjects([jsl rep:@"((fn (f x) (f x)) (fn (a) (+ 1 a)) 7)"], @"8");
    // closure
    XCTAssertEqualObjects([jsl rep:@"(((fn (a) (fn (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def gen-plus5 (fn () (fn (b) (+ 5 b))))"], @"user:gen-plus5/0");
    XCTAssertEqualObjects([jsl rep:@"(def plus5 (gen-plus5))"], @"user:plus5/1");
    XCTAssertEqualObjects([jsl rep:@"(plus5 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def gen-plusX (fn (x) (fn (b) (+ x b))))"], @"user:gen-plusX/1");
    XCTAssertEqualObjects([jsl rep:@"(def plus7 (gen-plusX 7))"], @"user:plus7/1");
    XCTAssertEqualObjects([jsl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"(def sumdown (fn (N) (if (> N 0) (+ N (sumdown  (- N 1))) 0)))"], @"user:sumdown/1");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 2)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 6)"], @"21");
    XCTAssertEqualObjects([jsl rep:@"(def fib (fn (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"], @"user:fib/1");
    XCTAssertEqualObjects([jsl rep:@"(fib 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(fib 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(fib 4)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) more) 2)"], @"(2)");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) (list? more)) 1 2 3)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) (count more)) 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) (count more)))"], @"0");
    XCTAssertEqualObjects([jsl rep:@"((fn (& more) (list? more)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"((fn (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"((fn (a & more) (count more)) 1)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"((fn (a & more) (list? more)) 1)"], @"true");
    // Test apply
    XCTAssertEqualObjects([jsl rep:@"(apply + 2 3)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply + 4 5 10)"], @"19");
    XCTAssertEqualObjects([jsl rep:@"(apply list [])"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (a b) (+ a b)) [2 3])"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (a b) (+ a b)) [4 5])"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (& more) (list? more)) [1 2 3])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (& more) (list? more)) [])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (a & more) (list? more)) [1])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (& form) (count form)) [1 2])"], @"2");
    // test bindings
    [jsl rep:@"(def a (fn (x) (let (y x z (* x x)) (+ y z))))"];
    XCTAssertEqualObjects([jsl rep:@"(a 10)"], @"110");
    // test anonymous function print
    XCTAssertEqualObjects([jsl rep:@"(fn (& more) 1)"], @"#<fn/n>");
    XCTAssertEqualObjects([jsl rep:@"(fn (a) 1)"], @"#<fn/1>");
    // symbols with quote in name
    XCTAssertEqualObjects([jsl rep:@"(def c' 10)"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(def f' (fn (x) (+ c' x)))"], @"user:f'/1");
    XCTAssertEqualObjects([jsl rep:@"(f' 7)"], @"17");
    // function as argument
    [jsl rep:@"(def x 3)"];
    [jsl rep:@"(defun identity (x) x)"];
    XCTAssertEqualObjects([jsl rep:@"(identity (fn (n) n))"], @"#<fn/1>");
    XCTAssertEqualObjects([jsl rep:@"(identity (fn (x y) 1))"], @"#<fn/2>");
    XCTAssertEqualObjects([jsl rep:@"(fn? (identity (fn (x y) 1)))"], @"true");
    // multi-arity
    XCTAssertEqualObjects([jsl rep:@"((fn (x) (let (x (fn (a b) (+ a b))) (x 1))) (fn (y) y))"], @"1");
    XCTAssertEqualObjects([jsl rep:@"((fn (x) (let (x (fn (a b) (+ a b))) (x 1 2))) (fn (y) y))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"((fn (x) (let (x (fn (a b) (+ a b))) (+ (x 1) (x 2 3)))) (fn (y) y))"], @"6");
}

- (void)testMultiArityFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def a (fn () 0))"];
    [jsl rep:@"(def a (fn (x) 1))"];
    XCTAssertEqualObjects([jsl rep:@"(a)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(a 3)"], @"1");
    // variadic function
    XCTAssertEqualObjects([jsl rep:@"(def x (fn (& more) more))"], @"user:x/n");
    XCTAssertEqualObjects([jsl rep:@"x/n"], @"user:x/n");
    XCTAssertEqualObjects([jsl rep:@"(first (x 1 2 3 4))"], @"1");
}

- (void)testNotFunction {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(not false)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not 0)"], @"false");
}

- (void)testOrFunction {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(or ())"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(or nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(or (= 1 1))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(or (= [1] [2]) (= (+ 1 1) 2))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(or (= 1 2) (= 3 4))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(or (= 1 2) (= 3 4) (= \"a\" \"a\"))"], @"true");
}

- (void)testAndFunction {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(and ())"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(and nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(and (= 1 1))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(and (= [1] [2]) (= (+ 1 1) 2))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(and (= [1] [1]) (= (+ 1 2) 2))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(and (= [1] [1]) (= (+ 1 1) 2))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(and (= 1 1) (= 4 4) (= -1 -1))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(and (= '(1) '(1)) true (= 4 4) (= \"a\" \"a\"))"], @"true");
}

- (void)testWhenMacro {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(def a (atom 0))"], @"(atom 0)");
    XCTAssertEqualObjects([jsl rep:@"(when (= 1 1) (reset! a 2) (reset! a (+ 2 @a)) 10)"], @"10");
    XCTAssertEqualObjects([jsl rep:@"@a"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(when (= 2 1) 10)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(when-not (= 2 1) (reset! a 20) (reset! a (+ 20 @a)) 100)"], @"100");
    XCTAssertEqualObjects([jsl rep:@"@a"], @"40");
    XCTAssertEqualObjects([jsl rep:@"(when-not (= 1 1) 10)"], @"nil");
}

- (void)testString {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"\"\""], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"\"abc\""], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"\"abc  def\""], @"\"abc  def\"");
    XCTAssertEqualObjects([jsl rep:@"\"\\\"\""], @"\"\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"\"abc\\ndef\\nghi\""], @"\"abc\\ndef\\nghi\"");
    XCTAssertEqualObjects([jsl rep:@"\"abc\\\\def\\\\ghi\""], @"\"abc\\\\def\\\\ghi\"");
    XCTAssertEqualObjects([jsl rep:@"\"\\\\n\""], @"\"\\\\n\"");
    XCTAssertEqualObjects([jsl rep:@"(seq \"abc\")"], @"(\"a\" \"b\" \"c\")");
    XCTAssertEqualObjects([jsl rep:@"(apply str (seq \"this is a test\"))"], @"\"this is a test\"");
    XCTAssertEqualObjects([jsl rep:@"(seq \"\")"], @"nil");
}

- (void)testStringCoreFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // first
    XCTAssertEqualObjects([jsl rep:@"(first \"\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(first \"abc\")"], @"\"a\"");
    XCTAssertEqualObjects([jsl rep:@"(first \"12abc\")"], @"\"1\"");
    // rest
    XCTAssertEqualObjects([jsl rep:@"(rest \"\")"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(rest \"a\")"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(rest \"ab\")"], @"[\"b\"]");
    XCTAssertEqualObjects([jsl rep:@"(rest \"abc\")"], @"[\"b\" \"c\"]");
    // nth
    XCTAssertEqualObjects([jsl rep:@"(nth 0 \"abc\")"], @"\"a\"");
    XCTAssertEqualObjects([jsl rep:@"(nth 1 \"abc\")"], @"\"b\"");
    XCTAssertThrows([jsl rep:@"(nth -1 \"abc\")"]);
    XCTAssertThrows([jsl rep:@"(nth 3 \"abc\")"]);
    XCTAssertThrows([jsl rep:@"(nth 0 \"\")"]);
    // take
    XCTAssertEqualObjects([jsl rep:@"(take 1 \"abc\")"], @"\"a\"");
    XCTAssertEqualObjects([jsl rep:@"(take 2 \"abc\")"], @"\"ab\"");
    XCTAssertEqualObjects([jsl rep:@"(take 0 \"\")"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(try (take 1 \"\") (catch ex ex))"], @"\"Index 1 is out of bound for length 0\"");
    XCTAssertEqualObjects([jsl rep:@"(try (take -1 \"abcd\") (catch ex ex))"], @"\"Index -1 is out of bound for length 0\"");
    XCTAssertEqualObjects([jsl rep:@"(try (take 10 \"abcd\") (catch ex ex))"], @"\"Index 10 is out of bound for length 4\"");
    // last
    XCTAssertEqualObjects([jsl rep:@"(last \"abc\")"], @"\"c\"");
    XCTAssertEqualObjects([jsl rep:@"(last \"\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(last \"a\")"], @"\"a\"");
    // drop
    XCTAssertEqualObjects([jsl rep:@"(drop 0 \"abc\")"], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"(drop 1 \"abc\")"], @"\"bc\"");
    XCTAssertEqualObjects([jsl rep:@"(drop 2 \"abc\")"], @"\"c\"");
    XCTAssertEqualObjects([jsl rep:@"(drop 3 \"abc\")"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(drop 4 \"abc\")"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(drop -1 \"abc\")"], @"\"\"");
    // reverse
    XCTAssertEqualObjects([jsl rep:@"(reverse \"abc\")"], @"\"cba\"");
    XCTAssertEqualObjects([jsl rep:@"(reverse \"a\")"], @"\"a\"");
    XCTAssertEqualObjects([jsl rep:@"(reverse \"\")"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(reverse \"1234\")"], @"\"4321\"");
    XCTAssertEqualObjects([jsl rep:@"(reverse \"[1 2 3 4]\")"], @"\"]4 3 2 1[\"");
    // nth-tail
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 0 0 \"abcd\")"], @"\"a\"");
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 0 2 \"abcd\")"], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 1 2 \"abcd\")"], @"\"bc\"");
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 3 3 \"abcd\")"], @"\"d\"");
    XCTAssertThrows([jsl rep:@"(nth-tail -1 3 \"abcd\")"]);
    XCTAssertThrows([jsl rep:@"(nth-tail 1 4 \"abcd\")"]);
    XCTAssertThrows([jsl rep:@"(nth-tail 1 0 \"abcd\")"]);
    // append
    XCTAssertEqualObjects([jsl rep:@"(append \"a\" 0 \"bcd\")"], @"\"abcd\"");
    XCTAssertEqualObjects([jsl rep:@"(append \"a\" 1 \"bcd\")"], @"\"bacd\"");
    XCTAssertEqualObjects([jsl rep:@"(append \"abc\" 0 \"d\")"], @"\"abcd\"");
    XCTAssertEqualObjects([jsl rep:@"(append \"ab\" 3 \"bcd\")"], @"\"bcdab\"");
    XCTAssertEqualObjects([jsl rep:@"(append 1 0 \"bcd\")"], @"\"1bcd\"");
    XCTAssertEqualObjects([jsl rep:@"(append '(1 2 3) 0 \"bcd\")"], @"\"(1 2 3)bcd\"");
    XCTAssertEqualObjects([jsl rep:@"(append [1 2 3] 0 \"bcd\")"], @"\"[1 2 3]bcd\"");
    XCTAssertEqualObjects([jsl rep:@"(append {:a 1} 0 \"bcd\")"], @"\"{:a 1}bcd\"");
    XCTAssertThrows([jsl rep:@"(append 0 4 '(1 2))"]);
}

- (void)testPrint {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(pr-str)"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str \"\")"], @"\"\\\"\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str \"\\\"\")"], @"\"\\\"\\\\\\\"\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"\"(1 2 \\\"abc\\\" \\\"\\\\\\\"\\\") \\\"def\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str \"abc\\\\ndef\\\\nghi\")"], @"\"\\\"abc\\\\\\\\ndef\\\\\\\\nghi\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"\"\\\"abc\\\\\\\\\\\\\\\\def\\\\\\\\\\\\\\\\ghi\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str (list))"], @"\"()\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str [(+ 21 12) 2 3])"], @"\"[33 2 3]\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str [1 2 \"abc\" \"\\\"\"] \"def\")"], @"\"[1 2 \\\"abc\\\" \\\"\\\\\\\"\\\"] \\\"def\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str [])"], @"\"[]\"");
    XCTAssertEqualObjects([jsl rep:@"(str [1 2 \"abc\" \"\\\"\"] \"def\")"], @"\"[1 2 abc \\\"]def\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"A\" {:abc \"val\"} \"Z\")"], @"\"A{:abc val}Z\"");
    XCTAssertEqualObjects([jsl rep:@"(str true \".\" false \".\" nil \".\" :keyw \".\" 'symb)"], @"\"true.false.nil.:keyw.user:symb\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str \"A\" {:abc \"val\"} \"Z\")"], @"\"\\\"A\\\" {:abc \\\"val\\\"} \\\"Z\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str true \".\" false \".\" nil \".\" :keyw \".\" 'symb)"],
                          @"\"true \\\".\\\" false \\\".\\\" nil \\\".\\\" :keyw \\\".\\\" user:symb\"");
    [jsl rep:@"(def s (str {:abc \"val1\" :def \"val2\"}))"];
    XCTAssertEqualObjects([jsl rep:@"(or (= s \"{:abc val1 :def val2}\") (= s \"{:def val2 :abc val1}\"))"], @"true");
    NSString *ret = [jsl rep:@"(def p (pr-str {:abc \"val1\" :def \"val2\"}))"];
    XCTAssertTrue([ret isEqual:@"\"{:abc \\\"val1\\\" :def \\\"val2\\\"}\""] || [ret isEqual:@"\"{:def \\\"val2\\\" :abc \\\"val1\\\"}\""]);
    XCTAssertEqualObjects([jsl rep:@"(or (= p \"{:abc \\\"val1\\\" :def \\\"val2\\\"}\") (= p \"{:def \\\"val2\\\" :abc \\\"val1\\\"}\"))"], @"true");
}

- (void)testStringFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(str)"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"\")"], @"\"\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"abc\")"], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"\\\"\")"], @"\"\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(str 1 \"abc\" 3)"], @"\"1abc3\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"abc  def\" \"ghi jkl\")"], @"\"abc  defghi jkl\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"abc\\\\ndef\\\\nghi\")"], @"\"abc\\\\ndef\\\\nghi\"");
    XCTAssertEqualObjects([jsl rep:@"(str \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"\"abc\\\\\\\\def\\\\\\\\ghi\"");
    XCTAssertEqualObjects([jsl rep:@"(str (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"\"(1 2 abc \\\")def\"");
    XCTAssertEqualObjects([jsl rep:@"(str (list))"], @"\"()\"");
    XCTAssertEqualObjects([jsl rep:@"(str [(+ 21 12) 2 3])"], @"\"[33 2 3]\"");
    XCTAssertEqualObjects([jsl rep:@"(str [(+ 21 12) 2 3 \"foo\"])"], @"\"[33 2 3 foo]\"");
    XCTAssertEqualObjects([jsl rep:@"(str [1 2 \"abc\" \"\\\"\"] \"def\")"], @"\"[1 2 abc \\\"]def\"");
    XCTAssertEqualObjects([jsl rep:@"(str [])"], @"\"[]\"");
    // testing list functions that works with string
    XCTAssertEqualObjects([jsl rep:@"(empty? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(empty? \"\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(count \"\")"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(count \"abc\")"], @"3");
}

- (void)testdoForm {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    MockStdIOService *stdIOService = [MockStdIOService new];
    [[jsl ioService] setStdIODelegate:stdIOService];
    XCTAssertEqualObjects([jsl rep:@"(do (def a 6) 7 (+ a 8))"], @"14");
    XCTAssertEqualObjects([jsl rep:@"a"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(def DO (fn (a) 7))"], @"user:DO/1");
    XCTAssertEqualObjects([jsl rep:@"(DO 3)"], @"7");
    // printing
    XCTAssertEqualObjects([jsl rep:@"(do (prn \"prn output1\"))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"prn output1\"");
    XCTAssertEqualObjects([jsl rep:@"(do (prn \"prn output2\") 7)"], @"7");
    XCTAssertEqualObjects([stdIOService output], @"\"prn output2\"");
    XCTAssertEqualObjects([jsl rep:@"(do (prn \"prn output1\") (prn \"prn output2\") (+ 1 2))"], @"3");
    XCTAssertEqualObjects([stdIOService output], @"\"prn output2\"");
}

- (void)testEquality {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(= (list 1 2 3) (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1 3 2) (list 1 2 3))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1 2 3) [1 2 3])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1 3 2) [1 2 3])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [1 2 3] [1 2 3])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [1 3 2] [1 2 3])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= [1 2 3] (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [1 3 2] (list 1 2 3))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 1 :b 2} {:a 1 :b 2})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 1 :b 2} {:b 2 :a 1})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 1 :b [1 2 3]} {:b [1 2 3] :a 1})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {:a 1 :b [1 0 3]} {:b [1 2 3] :a 1})"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= {\"a\" 1 :b [1 2 3]} {:b [1 2 3] \"a\" 1})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {\"a\" {:aa 11} :b [1 2 3]} {:b [1 2 3] \"a\" {:aa 11}})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 2 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 1 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 1 2)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 1 (+ 1 1))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 2 (+ 1 1))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= nil 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= nil nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 1 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 0 0)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 1 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"\" \"\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" \"abc\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"\" \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" \"def\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" \"ABC\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= true true)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= false false)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= nil nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list) (list))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1 2) (list 1 2))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (list 1) (list))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= (list) (list 1))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= 0 (list))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= (list) 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= (list) \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"\" (list))"], @"false");
    // test symbol equality
    XCTAssertEqualObjects([jsl rep:@"(= (quote abc) (quote abc))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (quote abc) (quote abcd))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= (quote abc) \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" (quote abc))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" (str (quote abc)))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= \"user:abc\" (str (quote abc)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (quote abc) nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= nil (quote abc))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not (= 1 1))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not (= 1 2))"], @"true");
    // test single argument
    XCTAssertEqualObjects([jsl rep:@"(= 0)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 1.2)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= -1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= -1.2)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= '())"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (atom 1))"], @"true");
    // testing muti arity
    XCTAssertEqualObjects([jsl rep:@"(= 1 1 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= 1.5 1.5 1.5)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= -1 -1 -1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= -1.5 -1.5 -1.5)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= -1 -1.5 -1.5)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= -1.5 -1 -1.5)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= -1.5 -1.5 -1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= '() '() '())"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [] [] [])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {} {} {})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= '(1) '(1) '(1))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [1] [1] [1])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= {1 2} {1 2} {1 2})"], @"true");
}

- (void)testNotEqual {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(not= [1] [1] [1])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not= 1.5 1.5 1.5)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not= [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not= (quote abc) (quote abc))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not= (quote abc) (quote def))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not= [1] [3])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not= \"abc\" \"def\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not= {:a 1} {:a 2})"], @"true");
}

- (void)testConditional {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(if (> (count (list 1 2 3)) 3) \"yes\" \"no\")"], @"\"no\"");
    XCTAssertEqualObjects([jsl rep:@"(if (> 3 3) \"yes\" \"no\")"], @"\"no\"");
    XCTAssertEqualObjects([jsl rep:@"(if (>= (count (list 1 2 3)) 3) \"yes\" \"no\")"], @"\"yes\"");
    XCTAssertEqualObjects([jsl rep:@"(if true 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(if false 7 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(if false 7 false)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(if true (+ 1 7) (+ 1 8))"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(if false (+ 1 7) (+ 1 8))"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(if nil 7 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(if 0 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(if \"\" 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(if (list) 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(if (list 1 2 3) 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(= (list) nil)"], @"false");
    // one-way if form
    XCTAssertEqualObjects([jsl rep:@"(if false (+ 1 7))"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(if nil 8 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(if true (+ 1 7))"], @"8");
}

- (void)testListCoreFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(list? (list))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(empty? (list))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(empty? (list 1))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(count (list 1 2 3))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(count (list))"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(count nil)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(apply str (seq \"this is a test\"))"], @"\"this is a test\"");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 (list))"], @"(1)");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 (list 2))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 (list 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(cons (list 1) (list 2 3))"], @"((1) 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(def a (list 2 3))"], @"(2 3)");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 a)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"a"], @"(2 3)");
    XCTAssertEqualObjects([jsl rep:@"(cons [1] [2 3])"], @"([1] 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 [2 3])"], @"(1 2 3)");
    // concat
    XCTAssertThrows([jsl rep:@"(concat)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(concat (list 1 2))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(concat (list 1 2) (list 3 4))"], @"(1 2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(concat (list 1 2) (list 3 4) (list 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertThrows([jsl rep:@"(concat (concat))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(concat (list) (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(concat \"abc\" \"xyz\")"], @"\"abcxyz\"");
    XCTAssertEqualObjects([jsl rep:@"(concat \"abc\" [1 2 3])"], @"\"abc123\"");
    XCTAssertEqualObjects([jsl rep:@"(concat \"abc\" [1 2 \"xyz\"])"], @"\"abc12xyz\"");
    XCTAssertEqualObjects([jsl rep:@"(concat \"abc\" '(1 2 \"xyz\"))"], @"\"abc12xyz\"");
    XCTAssertEqualObjects([jsl rep:@"(concat \"a\" [1 2 \"b\"] \"c\")"], @"\"a12bc\"");
    XCTAssertEqualObjects([jsl rep:@"(concat [1 2 3] [4 5 6])"], @"[1 2 3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(concat [1 2 3] '(4 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([jsl rep:@"(def a (list 1 2))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(def b (list 3 4))"], @"(3 4)");
    XCTAssertEqualObjects([jsl rep:@"(concat a b (list 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([jsl rep:@"a"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"b"], @"(3 4)");
    XCTAssertEqualObjects([jsl rep:@"(concat [1 2] (list 3 4) [5 6])"], @"(1 2 3 4 5 6)");
    // nth
    XCTAssertEqualObjects([jsl rep:@"(nth 0 (list 1))"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(nth 1 (list 1 2))"], @"2");
    [jsl rep:@"(def x \"x\")"];
    XCTAssertNotEqualObjects(@"(def x (nth (list 1 2) 2))", @"Index out of bounds");
    XCTAssertEqualObjects([jsl rep:@"x"], @"\"x\"");
    // first, rest
    XCTAssertEqualObjects([jsl rep:@"(first (list))"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(first (list 6))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(first (list 7 8 9))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(rest (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(rest (list 6))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(rest (list 7 8 9))"], @"(8 9)");
    XCTAssertEqualObjects([jsl rep:@"(first nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(rest nil)"], @"()");
    // or
    XCTAssertEqualObjects([jsl rep:@"(or)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(or 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(or 1 2 3 4)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(or false 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(or false nil 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(or false nil false false nil 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(or false nil 3 false nil 4)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(or (or false 4))"], @"4");
    // cond
    XCTAssertEqualObjects([jsl rep:@"(cond)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(cond true 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(cond true 7 true 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 true 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 \"else\" 9)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 (= 2 2) 8 \"else\" 9)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    // conj
    [jsl rep:@"(def xs '(1 2))"];
    XCTAssertEqualObjects([jsl rep:@"(conj (list) 1)"], @"(1)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 1) 2)"], @"(2 1)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 2 3) 4)"], @"(4 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 2 3) 4 5 6)"], @"(6 5 4 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 1) (list 2 3))"], @"((2 3) 1)");
    XCTAssertEqualObjects([jsl rep:@"(conj '(1 2 3) 4 5 6)"], @"(6 5 4 1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(conj xs 6)"], @"(6 1 2)");
    XCTAssertEqualObjects([jsl rep:@"xs"], @"(1 2)");
    XCTAssertThrows([jsl rep:@"(conj 1 '(1 2))"]);
    // seq
    XCTAssertEqualObjects([jsl rep:@"(seq '())"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(seq '(2 3 4))"], @"(2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(seq nil)"], @"nil");
    // last
    XCTAssertEqualObjects([jsl rep:@"(last [1 2 3 4])"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(last [1 2 3 [4]])"], @"[4]");
    XCTAssertEqualObjects([jsl rep:@"(last '(1 2 3 4))"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(last '(1 2 [3] [4 5]))"], @"[4 5]");
    XCTAssertEqualObjects([jsl rep:@"(last '())"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(last [])"], @"nil");
    XCTAssertThrows([jsl rep:@"(last nil)"]);
    // drop
    XCTAssertEqualObjects([jsl rep:@"(drop 1 '(1 2 3))"], @"(2 3)");
    XCTAssertEqualObjects([jsl rep:@"(drop 2 '(1 2 3))"], @"(3)");
    XCTAssertEqualObjects([jsl rep:@"(drop 3 '(1 2 3))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(drop 4 '(1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(drop -4 '(1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(drop -3 '(1 2 3))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(drop -2 '(1 2 3))"], @"(1)");
    XCTAssertEqualObjects([jsl rep:@"(drop -1 '(1 2 3))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(drop 0 '(1 2 3))"], @"(1 2 3)");
    // reverse
    XCTAssertEqualObjects([jsl rep:@"(reverse '())"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(reverse '(1 2 3))"], @"(3 2 1)");
    // nth-tail
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 0 0 '(0))"], @"(0)");
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 2 4 '(0 1 2 3 4))"], @"(2 3 4)");
    XCTAssertThrows([jsl rep:@"(nth-tail 5 10 '(0 1 2))"]);
    XCTAssertThrows([jsl rep:@"(nth-tail -1 2 '(0 1 2))"]);
    XCTAssertThrows([jsl rep:@"(nth-tail 1 3 '(0 1 2))"]);
    // take
    XCTAssertEqualObjects([jsl rep:@"(take 2 '(0 1 2 3))"], @"(0 1)");
    XCTAssertEqualObjects([jsl rep:@"(take 0 '(0 1 2 3))"], @"()");
    XCTAssertThrows([jsl rep:@"(take -1 '(0))"]);
    XCTAssertThrows([jsl rep:@"(take 2 '(0))"]);
    XCTAssertThrows([jsl rep:@"(take 2 nil)"]);
    XCTAssertThrows([jsl rep:@"(take 2 \"\"))"]);
    // append
    XCTAssertEqualObjects([jsl rep:@"(append 0 0 '(1))"], @"(0 1)");
    XCTAssertEqualObjects([jsl rep:@"(append 0 1 '(1))"], @"(1 0)");
    XCTAssertEqualObjects([jsl rep:@"(append 4 4 '(0 1 2 3 5))"], @"(0 1 2 3 4 5)");
    XCTAssertThrows([jsl rep:@"(append 0 -1 '(1 2))"]);
    XCTAssertThrows([jsl rep:@"(append 0 4 '(1 2))"]);
}

- (void)testVectorCoreFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a) (* 2 a)) [1 2 3])"], @"[2 4 6]");
    XCTAssertEqualObjects([jsl rep:@"(map (fn [& args] (list? args)) [1 2])"], @"[true true]");
    XCTAssertEqualObjects([jsl rep:@"(vector? [10 11])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(vector? '(12 13))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(vector 3 4 5)"], @"[3 4 5]");
    // conj
    [jsl rep:@"(def xs [1 2])"];
    XCTAssertEqualObjects([jsl rep:@"(conj [] 1)"], @"[1]");
    XCTAssertEqualObjects([jsl rep:@"(conj [1] 2)"], @"[1 2]");
    XCTAssertEqualObjects([jsl rep:@"(conj [2 3] 4)"], @"[2 3 4]");
    XCTAssertEqualObjects([jsl rep:@"(conj [2 3] 4 5 6)"], @"[2 3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(conj [1] [2 3])"], @"[1 [2 3]]");
    XCTAssertEqualObjects([jsl rep:@"(conj [1 2 3] 4 5 6)"], @"[1 2 3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(conj xs 6)"], @"[1 2 6]");
    XCTAssertEqualObjects([jsl rep:@"xs"], @"[1 2]");
    XCTAssertThrows([jsl rep:@"(conj 1 [1 2])"]);
    // nth
    XCTAssertEqualObjects([jsl rep:@"(nth 0 [1])"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(nth 1 [1 2])"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(def x \"x\")"], @"\"x\"");
    XCTAssertThrows([jsl rep:@"(def x (nth [1 2] 2))"], @"Index out of bounds");
    XCTAssertEqualObjects([jsl rep:@"x"], @"\"x\"");
    // first, rest
    XCTAssertEqualObjects([jsl rep:@"(first [])"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(first [10])"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(first [10 11 12])"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(rest [])"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(rest [10])"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(rest [10 11 12])"], @"[11 12]");
    // seq
    XCTAssertEqualObjects([jsl rep:@"(seq [2 3 4])"], @"(2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(seq [])"], @"nil");
    // drop
    XCTAssertEqualObjects([jsl rep:@"(drop 1 [1 2 3])"], @"[2 3]");
    XCTAssertEqualObjects([jsl rep:@"(drop 2 [1 2 3])"], @"[3]");
    XCTAssertEqualObjects([jsl rep:@"(drop 3 [1 2 3])"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(drop 4 [1 2 3])"], @"[1 2 3]");
    XCTAssertEqualObjects([jsl rep:@"(drop -4 [1 2 3])"], @"[1 2 3]");
    XCTAssertEqualObjects([jsl rep:@"(drop -3 [1 2 3])"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(drop -2 [1 2 3])"], @"[1]");
    XCTAssertEqualObjects([jsl rep:@"(drop -1 [1 2 3])"], @"[1 2]");
    XCTAssertEqualObjects([jsl rep:@"(drop 0 [1 2 3])"], @"[1 2 3]");
    // reverse
    XCTAssertEqualObjects([jsl rep:@"(reverse [])"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(reverse [1 2 3])"], @"[3 2 1]");
    // nth-tail
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 0 0 [0])"], @"[0]");
    XCTAssertEqualObjects([jsl rep:@"(nth-tail 2 4 [0 1 2 3 4])"], @"[2 3 4]");
    XCTAssertThrows([jsl rep:@"(nth-tail 5 10 [0 1 2])"]);
    // take
    XCTAssertEqualObjects([jsl rep:@"(take 2 '[0 1 2 3])"], @"[0 1]");
    XCTAssertEqualObjects([jsl rep:@"(take 0 '[0 1 2 3])"], @"[]");
    XCTAssertThrows([jsl rep:@"(take -1 '[0])"]);
    XCTAssertThrows([jsl rep:@"(take 2 '[0])"]);
    XCTAssertThrows([jsl rep:@"(take 2 nil)"]);
    XCTAssertThrows([jsl rep:@"(take 2 \"\"))"]);
    // append
    XCTAssertEqualObjects([jsl rep:@"(append 0 0 [1])"], @"[0 1]");
    XCTAssertEqualObjects([jsl rep:@"(append 0 1 [1])"], @"[1 0]");
    XCTAssertEqualObjects([jsl rep:@"(append 4 4 [0 1 2 3 5])"], @"[0 1 2 3 4 5]");
    XCTAssertThrows([jsl rep:@"(append 0 -1 [1 3])"]);
    XCTAssertThrows([jsl rep:@"(append 0 4 [1 2])"]);
    XCTAssertThrows([jsl rep:@"(append 0 [1 2])"]);
}

- (void)testKeyword {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(= :abc :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= :abc :def)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= :abc \":abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth 0 (keys {:abc 123 :def 456})))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@":1"], @":1");
    XCTAssertEqualObjects([jsl rep:@"(keyword? :1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(def a \"abc\")"], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"(keyword a)"], @":abc");
    // testing fail conditions
    XCTAssertEqualObjects([jsl rep:@"(keyword 1)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(keyword (atom 1))"], @"nil");
    XCTAssertThrows([jsl rep:@"(keyword xyz)"], @"'xyz' not found");
    XCTAssertEqualObjects([jsl rep:@"(keyword (atom 1))"], @"nil");
}

- (void)testQuote {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(quote 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(quote (1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(quote (1 2 (3 4)))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 2 (3 4)))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (nil))"], @"(nil)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (unquote 7))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(def a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (unquote a))"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 a 3))"], @"(1 user:a 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (unquote a) 3))"], @"(1 8 3)");
    XCTAssertEqualObjects([jsl rep:@"(def b (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 b 3))"], @"(1 user:b 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (unquote b) 3))"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote ((unquote 1) (unquote 2)))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(def c (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 c 3))"], @"(1 user:c 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (splice-unquote c) 3))"], @"(1 1 \"b\" \"d\" 3)");
    // testing reader macros
    XCTAssertEqualObjects([jsl rep:@"'7"], @"7");
    XCTAssertEqualObjects([jsl rep:@"'(1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"'(1 2 (3 4))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([jsl rep:@"`7"], @"7");
    XCTAssertEqualObjects([jsl rep:@"`(1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 2 (3 4))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([jsl rep:@"`(nil)"], @"(nil)");
    XCTAssertEqualObjects([jsl rep:@"`~7"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(def a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~a 3)"], @"(1 8 3)");
    XCTAssertEqualObjects([jsl rep:@"(def b '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`(1 b 3)"], @"(1 user:b 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~b 3)"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([jsl rep:@"(def c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`(1 c 3)"], @"(1 user:c 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~@c 3)"], @"(1 1 \"b\" \"d\" 3)");
    XCTAssertEqualObjects([jsl rep:@"(def a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"`[1 a 3]"], @"(1 user:a 3)");
    XCTAssertEqualObjects([jsl rep:@"[1 a 3]"], @"[1 8 3]");
    XCTAssertEqualObjects([jsl rep:@"(def c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`[1 ~@c 3]"], @"(1 1 \"b\" \"d\" 3)");
}

/** Testing macros without nesting */
- (void)testSimpleMacros {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmacro one () 1)"];
    XCTAssertEqualObjects([jsl rep:@"(one)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(macro? one/0)"], @"true");
    [jsl rep:@"(defmacro two () 2)"];
    XCTAssertEqualObjects([jsl rep:@"(two)"], @"2");
    [jsl rep:@"(defmacro identity (x) x)"];
    XCTAssertEqualObjects([jsl rep:@"(let (a 123) (identity a))"], @"123");
    [jsl rep:@"(defmacro foo (& more) `(count (list ~@more)))"];
    XCTAssertEqualObjects([jsl rep:@"(foo 1 2 3)"], @"3");
    // macro with hash-map
    [jsl rep:@"(defmacro hm (k v) `(hash-map ~k ~v))"];
    XCTAssertEqualObjects([jsl rep:@"(hm 1 '(3 4 5))"], @"{1 (3 4 5)}");
    [jsl rep:@"(defmacro hm1 (k v) `(let (x ~v) (hash-map ~k (first x))))"];
    XCTAssertEqualObjects([jsl rep:@"(hm1 1 '(3 4 5))"], @"{1 3}");
    [jsl rep:@"(defmacro p (x) `(let (z ~x) (list z 4 5 6 7)))"];
    XCTAssertEqualObjects([jsl rep:@"(p 3)"], @"(3 4 5 6 7)");
    XCTAssertEqualObjects([jsl rep:@"(hm 2 (p 3))"], @"{2 (3 4 5 6 7)}");
    XCTAssertEqualObjects([jsl rep:@"(hm1 2 (p 3))"], @"{2 3}");
    [jsl rep:@"(defmacro p (x) `(let (z (atom 3)) (list z 4 5 6 7)))"];
    XCTAssertEqualObjects([jsl rep:@"(hm :b @(first(p 3)))"], @"{:b 3}");
    XCTAssertEqualObjects([jsl rep:@"(hm1 :a (p 5))"], @"{:a (atom 3)}");
    // test macro definition print
    XCTAssertEqualObjects([jsl rep:@"(defmacro a (x) `(+ 1 ~x))"], @"user:a/1");
    XCTAssertEqualObjects([jsl rep:@"(defmacro a (& more) `(first (list ~@more)))"], @"user:a/n");
    // misc
    XCTAssertEqualObjects([jsl rep:@"(defun inc (x) (+ x 1))"], @"user:inc/1");
    XCTAssertEqualObjects([jsl rep:@"(defmacro apply1 (x) `(apply inc/1 ~x))"], @"user:apply1/1");
    XCTAssertEqualObjects([jsl rep:@"(apply1 [3])"], @"4");
    // vector binding in let
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo1 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([jsl rep:@"(foo1)"], @"11");
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo2 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(nth 1 xs) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([jsl rep:@"(foo2)"], @"12");
}

- (void)testMacroGensym {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmacro double (n) (let (x (gensym)) `(let (~x ~n) (+ ~x ~n))))"], @"user:double/1");
    XCTAssertEqualObjects([jsl rep:@"(double 1)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(double -1)"], @"-2");
    XCTAssertEqualObjects([jsl rep:@"(double 0)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(double 3.14)"], @"6.28");
}

- (void)testMacro {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmacro unless (pred a b) `(if ~pred ~b ~a))"];
    XCTAssertEqualObjects([jsl rep:@"(unless false 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(unless true 7 8)"], @"8");
    [jsl rep:@"(defmacro unless2 (pred a b) `(if (not ~pred) ~a ~b))"];
    XCTAssertEqualObjects([jsl rep:@"(unless2 false 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(unless2 true 7 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(macroexpand (unless2 2 3 4))"], @"(user:if (user:not 2) 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(= (gensym) (gensym))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(let [or_FIXME 23] (or false (+ or_FIXME 100)))"], @"123");
    [jsl rep:@"(defmacro pow2 (a) (let (x 2) `(* ~a ~x)))"];
    [jsl rep:@"(defun inc2 (x) (pow2 x))"];
    XCTAssertEqualObjects([jsl rep:@"(inc2 5)"], @"10");
    [jsl rep:@"(defmacro p1 (a) (let (x 10) `(* ~a ~x)))"];
    [jsl rep:@"(defmacro p2 (a) (let (x 2) `(p1 (* ~a ~x))))"];
    XCTAssertEqualObjects([jsl rep:@"(def n (fn (x) (p2 x)))"], @"user:n/1");
    XCTAssertEqualObjects([jsl rep:@"(n 4)"], @"80");
    XCTAssertEqualObjects([jsl rep:@"(n 10)"], @"200");
    [jsl rep:@"(defmacro p3 (a) (let (x 5) `(p2 (* ~a ~x))))"];
    [jsl rep:@"(def n (fn (x) (p3 x)))"];
    XCTAssertEqualObjects([jsl rep:@"(n 4)"], @"400");
    XCTAssertEqualObjects([jsl rep:@"(n 5)"], @"500");
    XCTAssertEqualObjects([jsl rep:@"`local-sym#"], @"user:local-sym#");
}

- (void)testVectorBasedMacros {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo1 () `(let (xs [(atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2)))]) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo1 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([jsl rep:@"(foo1)"], @"11");
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo2 () `(let (xs [(atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2)))]) (@(nth 1 xs) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo2 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(nth 1 xs) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([jsl rep:@"(foo2)"], @"12");
}

- (void)testErrorHandling {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    MockStdIOService *stdIOService = [MockStdIOService new];
    [[jsl ioService] setStdIODelegate:stdIOService];
    XCTAssertThrows([jsl rep:@"(throw \"err1\")"], @"Error: err1");
    XCTAssertThrows([jsl rep:@"(throw {:msg \"err2\"})"], @"Error: {:msg \"err2\"}");
    XCTAssertEqualObjects([jsl rep:@"(try 123 (catch e 456))"], @"123");
    @try {
        XCTAssertEqualObjects([jsl rep:@"(try (abc 1 2) (catch exc (prn \"exc is:\" exc)))"], @"nil");
    } @catch (NSException *exception) {
        XCTAssertEqualObjects([jsl printException:exception log:NO readably:YES], @"exc is:" "'abc' not found");
    }
    XCTAssertEqualObjects([jsl rep:@"(try (throw \"my exception\") (catch exc (do (prn \"exc:\" exc) 7)))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(try (abc 1 2) (catch exc (prn \"exc is:\" exc)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"exc is:\" \"'user:abc/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(try (nth 1 []) (catch exc (prn \"exc is:\" exc)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"exc is:\" \"Index 1 is out of bound for length 0\"");
    XCTAssertEqualObjects([jsl rep:@"(try (map throw/1 (list \"my err\")) (catch exc exc))"], @"\"my err\"");
    XCTAssertEqualObjects([jsl rep:@"(try (throw [\"data\" \"foo\"]) (catch exc (do (prn \"exc is:\" exc) 7)))"], @"7");
    XCTAssertEqualObjects([stdIOService output], @"\"exc is:\" [\"data\" \"foo\"]");
    XCTAssertThrows([jsl rep:@"(try xyz)"], @"'xyz' not found");
    XCTAssertEqualObjects([jsl rep:@"(try (throw (list 1 2 3)) (catch exc (do (prn \"err:\" exc) 7)))"], @"7");
    XCTAssertEqualObjects([stdIOService output], @"\"err:\" (1 2 3)");
}

- (void)testMeta {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@""];
    XCTAssertFalse([sym hasMeta]);
    id<JSDataProtocol> meta = [[JSString alloc] initWithString:@"meta-string"];
    JSSymbol *symMeta = [[JSSymbol alloc] initWithMeta:meta symbol:sym];
    XCTAssertNotNil([symMeta meta]);
    XCTAssertTrue([symMeta hasMeta]);
    XCTAssertNil([sym meta]);
    XCTAssertFalse([sym hasMeta]);
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta [1 2 3] {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (fn (a) a))"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (fn (a) a) {\"b\" 1}))"], @"{\"b\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (fn (a) a) \"abc\"))"], @"\"abc\"");
    [jsl rep:@"(def l-wm (with-meta (fn (a) a) {\"b\" 2}))"];
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm/1)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta l-wm/1 {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm/1)"], @"{\"b\" 2}");
    [jsl rep:@"(def f-wm (with-meta (fn [a] (+ 1 a)) {\"abc\" 1}))"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm/1)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta f-wm/1 {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm/1)"], @"{\"abc\" 1}");
    [jsl rep:@"(def f-wm2 ^{\"abc\" 1} (fn [a] (+ 1 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm2/1)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
    // testing closures and metadata
    [jsl rep:@"(def gen-plusX (fn (x) (with-meta (fn (b) (+ x b)) {\"meta\" 1})))"];
    [jsl rep:@"(def plus7 (gen-plusX 7))"];
    [jsl rep:@"(def plus8 (gen-plusX 8))"];
    XCTAssertEqualObjects([jsl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"(meta plus7/1)"], @"{\"meta\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta plus8/1)"], @"{\"meta\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta plus7/1 {\"meta\" 2}))"], @"{\"meta\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta plus8/1)"], @"{\"meta\" 1}");
    // testing metadata on collection
    XCTAssertEqualObjects([jsl rep:@"(meta [1 2 3])"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(with-meta [1 2 3] {\"a\" 1})"], @"[1 2 3]");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta [1 2 3] {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(vector? (with-meta [1 2 3] {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta [1 2 3] \"abc\"))"], @"\"abc\"");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (list 1 2 3) {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(list? (with-meta (list 1 2 3) {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta {\"abc\" 123} {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? (with-meta {\"abc\" 123} {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (atom 7) {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(def l-wm (with-meta [4 5 6] {\"b\" 2}))"], @"[4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta l-wm {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    // testing metadata on builtin functions
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
    [jsl rep:@"(def f-wm3 ^{\"fm\" 2} +)"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm3/n)"], @"{\"fm\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
}

/** Tests meta associated with the bound symbol */
- (void)testMetaForSymbol {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(def a (with-meta (fn (n) 1) [123]))"], @"user:a/1");
    XCTAssertEqualObjects([jsl rep:@"(meta a/1)"], @"[123]");
    XCTAssertEqualObjects([jsl rep:@"(meta (first (:exports (module-info \"user\"))))"], @"[123]");  // meta copied to the bound symbol
    XCTAssertEqualObjects([jsl rep:@"(meta (eval (first (:exports (module-info \"user\")))))"], @"[123]");  // meta associated with the value
    XCTAssertEqualObjects([jsl rep:@"(defmodule bar (export all))"], @"bar");
    XCTAssertEqualObjects([jsl rep:@"(defmacro ml (n) (with-meta 1 [:a :b :c]))"], @"bar:ml/1");
    XCTAssertEqualObjects([jsl rep:@"(meta ml/1)"], @"[:a :b :c]");
    XCTAssertEqualObjects([jsl rep:@"(meta (first (:exports (module-info \"bar\"))))"], @"[:a :b :c]");
    XCTAssertEqualObjects([jsl rep:@"(meta (eval (first (:exports (module-info \"bar\")))))"], @"[:a :b :c]");
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo (export all))"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(defmacro m () (with-meta (fn (n) 1) [:a :b :c]))"], @"foo:m/0");
    XCTAssertEqualObjects([jsl rep:@"(meta (first (:exports (module-info \"foo\"))))"], @"[:a :b :c]");
    XCTAssertEqualObjects([jsl rep:@"(meta (eval (first (:exports (module-info \"foo\")))))"], @"[:a :b :c]");
}

- (void)testTCO {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def sum2 (fn (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))"];  // tail recursive
    XCTAssertEqualObjects([jsl rep:@"(sum2 10 0)"], @"55");
    XCTAssertEqualObjects([jsl rep:@"(def res2 nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(def res2 (sum2 10000 0))"], @"50005000");
    [jsl rep:@"(def foo (fn (n) (if (= n 0) 0 (bar (- n 1)))))"];
    [jsl rep:@"(def bar (fn (n) (if (= n 0) 0 (foo (- n 1)))))"];
    XCTAssertEqualObjects([jsl rep:@"(foo 10000)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(do (do 1 2))"], @"2");
    [jsl rep:@"(def g (fn [] 78))"];
    XCTAssertEqualObjects([jsl rep:@"(g)"], @"78");
    [jsl rep:@"(def g (fn [a] (+ a 78)))"];
    XCTAssertEqualObjects([jsl rep:@"(g 3)"], @"81");
}

/** Returns the file from the current bundle's resources. */
- (NSString *)pathForFile:(NSString *)filename {
    NSString *path = [[NSBundle bundleForClass:[self class]] resourcePath];
    return [NSString stringWithFormat:@"%@/%@", path, filename];
}

- (void)testReadString {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    FileOps *fops = [FileOps new];
    XCTAssertEqualObjects([jsl rep:@"(read-string \"(1 2 (3 4) nil)\")"], @"(1 2 (3 4) nil)");
    // Internally qualifies with current module name if not qualified. Eval will check core functions in core module if not in user module unless they are
    // explicitly qualified.
    XCTAssertEqualObjects([jsl rep:@"(read-string \"(+ 2 3)\")"], @"(user:+ 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(read-string \"7 ;; comment\")"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(read-string \";; comment\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(eval (read-string \"(+ 2 3)\"))"], @"5");
    [fops createFileIfNotExist:@"/tmp/jsl-test.txt"];
    [fops append:@"A line of text\n" completion: ^{
        XCTAssertEqualObjects([jsl rep:@"(slurp \"/tmp/jsl-test.txt\")"], @"\"A line of text\\n\"");
        [fops closeFile];
        [fops delete:@"/tmp/jsl-test.txt"];
    }];
    XCTAssertEqualObjects([jsl rep:@"(read-string \";\")"], @"nil");
    // File read exception
    @try {
        [jsl rep:@"(slurp \"foo\")"];
    } @catch (NSException *excep) {
        XCTAssertTrue([Utils matchString:[jsl printException:excep log:YES readably:YES] withPattern:@".*No such file or directory.*"]);
    }
}

- (void)testAtom {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def inc3 (fn (a) (+ 3 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(def a (atom 2))"], @"(atom 2)");
    XCTAssertEqualObjects([jsl rep:@"(atom? a)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(atom? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(reset! a 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(swap! a inc3/1)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn (a) a))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn (a) (* 2 a)))"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn (a b) (* a b)) 10)"], @"120");
    XCTAssertEqualObjects([jsl rep:@"(swap! a + 3)"], @"123");
    // testing swap! closure interaction
    [jsl rep:@"(def inc-it (fn (a) (+ 1 a)))"];
    [jsl rep:@"(def atm (atom 7))"];
    [jsl rep:@"(def f (fn () (swap! atm inc-it/1)))"];
    XCTAssertEqualObjects([jsl rep:@"(f)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(f)"], @"9");
    // testing `@` deref reader macro
    XCTAssertEqualObjects([jsl rep:@"(def atm (atom 9))"], @"(atom 9)");
    XCTAssertEqualObjects([jsl rep:@"@atm"], @"9");
    NSString *ret = [jsl rep:@"(def a (atom {:x 1 :y 2}))"];
    XCTAssertTrue([ret isEqual:@"(atom {:x 1 :y 2})"] || [ret isEqual:@"(atom {:y 2 :x 1})"]);
    XCTAssertEqualObjects([jsl rep:@"(get :x @a)"], @"1");
    ret = [jsl rep:@"(reset! a {:x 1 :y (+ (get :y @a) 1)})"];
    XCTAssertTrue([ret isEqual:@"{:x 1 :y 3}"] || [ret isEqual:@"{:y 3 :x 1}"]);
    XCTAssertEqualObjects([jsl rep:@"(def a (atom {}))"], @"(atom {})");
    XCTAssertEqualObjects([jsl rep:@"(assoc @a :z 1)"], @"{:z 1}");
    [jsl rep:@"(def e (atom {\"+\" +}))"];
    [jsl rep:@"(swap! e assoc \"-\" -)"];
    XCTAssertEqualObjects([jsl rep:@"((get \"+\" @e) 7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"((get \"-\" @e) 11 8)"], @"3");
    [jsl rep:@"(swap! e assoc \"foo\" (list))"];
    XCTAssertEqualObjects([jsl rep:@"(get \"foo\" @e)"], @"()");
    [jsl rep:@"(swap! e assoc \"bar\" '(1 2 3))"];
    XCTAssertEqualObjects([jsl rep:@"(get \"bar\" @e)"], @"(1 2 3)");
}

- (void)testEval {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // testing eval does not use local environment
    XCTAssertEqualObjects([jsl rep:@"(def a 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(let (a 2) (eval (read-string \"a\")))"], @"1");
}

- (void)testPredicate {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    MockStdIOService *stdIOService = [MockStdIOService new];
    [[jsl ioService] setStdIODelegate:stdIOService];
    // nil?
    XCTAssertEqualObjects([jsl rep:@"(nil? nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(nil? true)"], @"false");
    // true?
    XCTAssertEqualObjects([jsl rep:@"(true? true)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(true? false)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? true?/1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? :a)"], @"false");
    // false?
    XCTAssertEqualObjects([jsl rep:@"(false? false)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(false? true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? :a)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(apply prn (list 1 2 \"3\" (list)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"1 2 \"3\" ()");
    XCTAssertEqualObjects([jsl rep:@"(apply prn/n 1 2 (list \"3\" (list)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"1 2 (\"3\" ())");
    XCTAssertEqualObjects([jsl rep:@"(apply prn/n 1 2 [\"3\" 4])"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"1 2 [\"3\" 4]");
    XCTAssertEqualObjects([jsl rep:@"(apply list (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(apply symbol?/1 (list (quote two)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (a b) (+ a b)) (list 2 3))"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn (a b) (+ a b)) (list 4 5))"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(def nums (list 1 2 3))"], @"(1 2 3)");
    [jsl rep:@"(def double (fn (a) (* 2 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(double 3)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(map double/1 nums)"], @"(2 4 6)");
    // symbol?
    XCTAssertEqualObjects([jsl rep:@"(map (fn (x) (symbol? x)) (list 1 (quote two) \"three\"))"], @"(false true false)");
    XCTAssertEqualObjects([jsl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? :abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? (symbol \"abc\"))"], @"true");
    // keyword?
    XCTAssertEqualObjects([jsl rep:@"(keyword? :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"abc\")"], @"*:abc");
    XCTAssertEqualObjects([jsl rep:@"(keyword :abc)"], @":abc");
    XCTAssertEqualObjects([jsl rep:@"(keyword \"abc\")"], @":abc");
    // seq?
    XCTAssertEqualObjects([jsl rep:@"(seq? (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(seq? [15])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(seq? seq?/1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(seq? nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(seq? \"abc\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(seq? {:a 1 :b 2})"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(seq? 1)"], @"false");
    // hash-map?
    XCTAssertEqualObjects([jsl rep:@"(hash-map? {})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? '())"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(hash-map? :abc)"], @"false");
    // string?
    XCTAssertEqualObjects([jsl rep:@"(string? \"\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(string? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? \"abc\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(string? :abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? (keyword \"abc\"))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? 234)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? nil)"], @"false");
    // number?
    XCTAssertEqualObjects([jsl rep:@"(number? 123)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(number? -1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(number? nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(number? false)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(number? \"123\")"], @"false");
    // fn?
    [jsl rep:@"(def add1 (fn (x) (+ x 1)))"];
    XCTAssertEqualObjects([jsl rep:@"(fn? +)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(fn? add1/1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(fn? cond)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(fn? \"+\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(fn? :+)"], @"false");
    // macro?
    XCTAssertEqualObjects([jsl rep:@"(macro? cond)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(macro? +)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? add1/1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? \"+\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? :+)"], @"false");
    // zero?
    XCTAssertEqualObjects([jsl rep:@"(zero? 0)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(zero? -1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(zero? (+ 1 -1))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(zero? (* 1 0))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(zero? 1)"], @"false");
    // coll?
    XCTAssertEqualObjects([jsl rep:@"(coll? [])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(coll? [1 2])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(coll? '())"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(coll? '(1 2))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(coll? {})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(coll? {:a 1})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(coll? \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(coll? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(coll? nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(coll? (atom 0))"], @"false");
    // contains? on list
    XCTAssertEqualObjects([jsl rep:@"(contains? 1 '(0 1 2))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? -1 '(0 -1 -2))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? 10 '(0 1 2))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(contains? nil '(0 1 2))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(contains? nil '(0 nil 2))"], @"true");
    // contains? on vector
    XCTAssertEqualObjects([jsl rep:@"(contains? 1 [0 1 2])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? -1 [0 -1 -2])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? 10 [0 1 2])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(contains? nil [0 1 2])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(contains? nil [0 nil 2])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"a\" [0 nil 2 \"a\"])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"a\" [0 nil 2 \"abc\"])"], @"false");
    // contains? on string
    XCTAssertEqualObjects([jsl rep:@"(contains? \"a\" \"abc\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"ab\" \"abc\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? \"ab\" \"acb\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(contains? 1 \"ac1b\")"], @"true");
    // even?
    XCTAssertEqualObjects([jsl rep:@"(even? 0)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(even? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(even? 2)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(even? 3)"], @"false");
    XCTAssertThrows([jsl rep:@"(even? 1.1)"]);
    XCTAssertThrows([jsl rep:@"(even? 1.2)"]);
    XCTAssertThrows([jsl rep:@"(even? 2.2)"]);
    // odd?
    XCTAssertEqualObjects([jsl rep:@"(odd? 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(odd? 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(odd? 2)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(odd? 3)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(odd? 4)"], @"false");
    XCTAssertThrows([jsl rep:@"(odd? 1.1)"]);
    XCTAssertThrows([jsl rep:@"(odd? 1.2)"]);
    XCTAssertThrows([jsl rep:@"(odd? 2.2)"]);
}

- (void)testMisc {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"nil"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"*host-language*"], @"\"Objective-C 2.0\"");
    [jsl rep:@"(def start-time (time-ms))"];
    XCTAssertEqualObjects([jsl rep:@"(= start-time 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(let [sumdown (fn (N) (if (> N 0) (+ N (sumdown (- N 1))) 0))] (sumdown 100)) ; Waste some time"], @"5050");  // not tail recursive
    XCTAssertEqualObjects([jsl rep:@"(> (time-ms) start-time)"], @"true");
    // Object updates
    JSSymbol *s = [[JSSymbol alloc] initWithName:@"foo"];
    [s setModuleName:@"s"];
    JSSymbol *a = s;
    [a setModuleName:@"a"];
    XCTAssertNotEqualObjects([s moduleName], @"s");
    a = [s copy];
    [a setModuleName:@"b"];
    XCTAssertNotEqualObjects([s moduleName], @"s");
}

- (void)testTypeFunction {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(type \"a\")"], @"\"string\"");
    XCTAssertEqualObjects([jsl rep:@"(type 1)"], @"\"number\"");
    XCTAssertEqualObjects([jsl rep:@"(type 3.14)"], @"\"number\"");
    XCTAssertEqualObjects([jsl rep:@"(type +)"], @"\"function\"");
    XCTAssertEqualObjects([jsl rep:@"(type when)"], @"\"function\"");
    XCTAssertEqualObjects([jsl rep:@"(type 'a)"], @"\"symbol\"");
    XCTAssertEqualObjects([jsl rep:@"(type [1 2])"], @"\"vector\"");
    XCTAssertEqualObjects([jsl rep:@"(type '(1 2))"], @"\"list\"");
    XCTAssertEqualObjects([jsl rep:@"(type {:a 1})"], @"\"hash-map\"");
    XCTAssertEqualObjects([jsl rep:@"(type :a)"], @"\"keyword\"");
    XCTAssertEqualObjects([jsl rep:@"(type (atom 1))"], @"\"atom\"");
    XCTAssertEqualObjects([jsl rep:@"(type (fn () 1))"], @"\"function\"");
    XCTAssertEqualObjects([jsl rep:@"(def a 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(type a)"], @"\"number\"");
    XCTAssertEqualObjects([jsl rep:@"(type nil)"], @"\"nil\"");
}

- (void)testErrorMessages {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // data type error
    XCTAssertEqualObjects([jsl rep:@"(try (+ \"\") (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'string'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (+ []) (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'vector'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (+ [] \"\") (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'vector'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (+ '()) (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'list'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (empty? 1) (catch ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'number'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (empty? 1.0) (catch ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'number'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (empty? (atom 1)) (catch ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'atom'\"");
    XCTAssertEqualObjects([jsl rep:@"(try (first true) (catch ex (str ex)))"], @"\"'first/1' requires 'sequence' for argument 1 but obtained 'bool'\"");
    // Function not found
    XCTAssertEqualObjects([jsl rep:@"(try (abc [1 2 3]) (catch ex (str ex)))"], @"\"'user:abc/1' not found\"");
    // Arity error
    XCTAssertEqualObjects([jsl rep:@"(try (empty? [] []) (catch ex (str ex)))"], @"\"'user:empty?/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(try (list? [] []) (catch ex (str ex)))"], @"\"'user:list?/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(try (true? [] []) (catch ex (str ex)))"], @"\"'user:true?/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(def a (atom 4))"], @"(atom 4)");
    XCTAssertEqualObjects([jsl rep:@"(try (reset! x 4) (catch ex (str ex)))"], @"\"'user:x' not found\"");
}

/** The expressions loaded and evaluated from file should be added to the env just like it is evaluated from REPL. */
- (void)testScopeWithFile {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    NSString *scopePath = [self pathForFile:@"scope.jsl"];
    XCTAssertTrue([scopePath isNotEmpty]);
    NSString *ret = [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", scopePath]];
    XCTAssertEqualObjects(ret, @"[:ok \"scope.jsl\"]");
    XCTAssertEqualObjects([jsl rep:@"(f1 4 6)"], @"30");
}

- (void)testScope {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(def a 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(def c 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def d (atom 3))"], @"(atom 3)");
    [jsl rep:@"(def f1 (fn (a b) (do (+ a b c (deref d)) (let (x 6 z (+ a c) f (fn (x) (* x x))) (* x z)))))"];
    XCTAssertEqualObjects([jsl rep:@"(f1 4 6)"], @"30");
}

- (void)testModule {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))"];
    [jsl rep:@"(def a 1)"];
    XCTAssertEqualObjects([jsl rep:@"a"], @"1");
    [jsl rep:@"(in-module \"user\")"];
}

- (void)testMacroWithModules {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmacro d (n v) `(def ~n ~v))"];
    XCTAssertEqualObjects([jsl rep:@"(d x 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo ())"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(user:d t 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    [jsl rep:@"(in-module \"user\")"];
    XCTAssertThrows([jsl rep:@"(foo:random-1 4)"]);  // function not exported
}

- (void)testModuleExports {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo (export (inc 1)) (export (dec 1)))"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([jsl rep:@"(inc 4)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([jsl rep:@"(defun greet () 42)"], @"foo:greet/0");
    XCTAssertEqualObjects([jsl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(foo:inc 4)"], @"5");
    XCTAssertThrows([jsl rep:@"(foo:random-2)"]);  // function not exported
}

- (void)testModuleExportAll {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo (export (inc 1)) (export all))"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([jsl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([jsl rep:@"(defun greet () 42)"], @"foo:greet/0");
    XCTAssertEqualObjects([jsl rep:@"(inc 4)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(dec 4)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(foo:inc 41)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(foo:dec 43)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(foo:greet)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(defmodule bar (export (sum 2)))"], @"bar");
    XCTAssertEqualObjects([jsl rep:@"(defun sum (x y) (+ x y))"], @"bar:sum/2");
    XCTAssertEqualObjects([jsl rep:@"(defun diff (x y) (- x y))"], @"bar:diff/2");
    XCTAssertEqualObjects([jsl rep:@"(sum 40 2)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(diff 45 3)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(bar:sum 40 2)"], @"42");
    XCTAssertThrows([jsl rep:@"(bar:diff 45 3)"]);  // function not exported
    XCTAssertEqualObjects([jsl rep:@"(in-module \"foo\")"], @"\"foo\"");
    XCTAssertEqualObjects([jsl rep:@"(bar:sum 40 2)"], @"42");
    XCTAssertThrows([jsl rep:@"(bar:diff 45 3)"]);  // function not exported
}

- (void)testModuleImport {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule bar (export (sum 2) (sum 1)))"], @"bar");
    XCTAssertEqualObjects([jsl rep:@"(defun sum (x y) (+ x y))"], @"bar:sum/2");
    XCTAssertEqualObjects([jsl rep:@"(defun sum (x) (+ x 10))"], @"bar:sum/1");
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo (export (inc 1) (greet 0) (a 0)) (import (from bar (sum 2) (sum 1))))"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([jsl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([jsl rep:@"(defun greet () (sum 32))"], @"foo:greet/0");
    XCTAssertEqualObjects([jsl rep:@"(defmacro a () 1)"], @"foo:a/0");
    XCTAssertEqualObjects([jsl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(foo:greet)"], @"42");
    // Invoke MFA from string format
    XCTAssertEqualObjects([jsl rep:@"(eval (read-string \"(foo:greet)\"))"], @"42");
}

- (void)testRemoveModule {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule rfoo (export (sum 2)))"], @"rfoo");
    XCTAssertEqualObjects([jsl rep:@"(defun sum (x y) (+ x y))"], @"rfoo:sum/2");
    XCTAssertEqualObjects([jsl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(rfoo:sum 10 20)"], @"30");
    XCTAssertEqualObjects([jsl rep:@"(remove-module \"rfoo\")"], @"nil");
    XCTAssertThrows([jsl rep:@"(rfoo:sum 45 3)"]);
    XCTAssertThrows([jsl rep:@"(in-module \"rfoo\")"]);
}

- (void)testExportSymbolResolveFault {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmodule foo (export all))"];
    XCTAssertEqualObjects([State currentModuleName], @"foo");
    Env *fooEnv = [Env forModuleName:@"foo"];
    XCTAssertNotNil(fooEnv);
    XCTAssertEqualObjects([jsl rep:@"(defun fa (n) n)"], @"foo:fa/1");
    XCTAssertEqualObjects([jsl rep:@"(fa 21)"], @"21");
    [jsl rep:@"(in-module \"user\")"];
    XCTAssertEqualObjects([jsl rep:@"(foo:fa 21)"], @"21");
    [jsl rep:@"(defmodule bar (export (ba 1) (bb 1)))"];
    XCTAssertEqualObjects([State currentModuleName], @"bar");
    Env *barEnv = [Env forModuleName:@"bar"];
    XCTAssertNotNil(barEnv);
    NSArray *barKeys = [[barEnv exportTable] allKeys];
    XCTAssertEqual([barKeys count], 2);
    JSSymbol *sym = [barKeys firstObject];
    id<JSDataProtocol> elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertTrue([JSFault isFault:elem]);
    XCTAssertEqualObjects([jsl rep:@"(defun ba (n) (+ n 1))"], @"bar:ba/1");
    XCTAssertEqualObjects([jsl rep:@"(defun bb (n) (- n 1))"], @"bar:bb/1");
    XCTAssertEqualObjects([jsl rep:@"(ba 21)"], @"22");
    XCTAssertEqualObjects([jsl rep:@"(bb 21)"], @"20");
    [jsl rep:@"(in-module \"foo\")"];
    [fooEnv resolveFault:elem forKey:sym inEnv:barEnv];
    elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertFalse([JSFault isFault:elem]);
    XCTAssertEqualObjects([jsl rep:@"(bar:ba 21)"], @"22");
    sym = [[barKeys filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.value contains [c] %@", @"bb"]] firstObject];
    elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertTrue([JSFault isFault:elem]);
    XCTAssertEqualObjects([jsl rep:@"(bar:bb 21)"], @"20");
    sym = [[[[barEnv exportTable] allKeys] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.value contains [c] %@", @"bb"]] firstObject];
    elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertFalse([JSFault isFault:elem]);
    [jsl rep:@"(in-module \"user\")"];
    XCTAssertEqualObjects([jsl rep:@"(bar:ba 21)"], @"22");
    XCTAssertEqualObjects([jsl rep:@"(bar:bb 21)"], @"20");
}

- (void)testCodeLoadedFromFile {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    NSString *moduleTest = [self pathForFile:@"module-test.jsl"];
    XCTAssertTrue([moduleTest isNotEmpty]);
    NSString *ret = [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", moduleTest]];
    XCTAssertEqualObjects(ret, @"[:ok \"module-test.jsl\"]");
    XCTAssertEqualObjects([jsl rep:@"(bbar:bba 5)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(bbaz:zza 5)"], @"25");
    XCTAssertEqualObjects([jsl rep:@"(bbaz:zzb 5)"], @"30");
    XCTAssertEqualObjects([jsl rep:@"(bbaz:zzc 5)"], @"30");
}

- (void)testModuleFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // Test module-info
    XCTAssertEqualObjects([jsl rep:@"(defmodule ifoo (export (ifa 1) (ifa 2)) (import (from core (empty? 1))))"], @"ifoo");
    [jsl rep:@"(defun iinc (n) (+ n 1))"];
    [jsl rep:@"(def info (module-info \"ifoo\"))"];
    NSString *count = [jsl rep:@"(count (get :exports info))"];
    XCTAssertEqual([count integerValue], 2);
    count = [jsl rep:@"(count (get :imports info))"];
    XCTAssertEqual([count integerValue], 1);
    count = [jsl rep:@"(count (get :internal info))"];
    XCTAssertEqual([count integerValue], 1);
    XCTAssertEqualObjects([jsl rep:@"(current-module-name)"], @"\"ifoo\"");
    XCTAssertThrows([jsl rep:@"(in-module \"nope.ifoo\")"]);
    // Test module-info
    XCTAssertEqualObjects([jsl rep:@"(:name (module-info (current-module-name)))"], @"\"ifoo\"");
    // Test module-exist?
    XCTAssertEqualObjects([jsl rep:@"(module-exist? \"nope.ifoo\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(module-exist? \"core\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(module-exist? \"user\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(module-exist? \"ifoo\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(remove-module \"ifoo\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(module-exist? \"ifoo\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(current-module-name)"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(in-module \"core\")"], @"\"core\"");
    XCTAssertEqualObjects([jsl rep:@"(current-module-name)"], @"\"core\"");
    XCTAssertEqualObjects([jsl rep:@"(def mod-name \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(in-module mod-name)"], @"\"user\"");
    // Test all-modules
    [jsl rep:@"(def modules (all-modules))"];
    XCTAssertGreaterThanOrEqual([[jsl rep:@"(count modules)"] integerValue], 2);
    XCTAssertEqualObjects([jsl rep:@"(contains? \"core\" modules)"], @"true");
}

- (void)testModuleDescription {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule mdesc \"A test module.\" (export all))"], @"mdesc");
    Env *mdescEnv = [Env forModuleName:@"mdesc"];
    XCTAssertEqualObjects([mdescEnv moduleDescription], @"A test module.");
    [jsl rep:@"(def info (module-info \"mdesc\"))"];
    XCTAssertEqualObjects([jsl rep:@"(get :description info)"], @"\"A test module.\"");
}

- (void)testModuleArity {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo.arity (export (foo 1) (bar n)))"], @"foo.arity");
    XCTAssertEqualObjects([jsl rep:@"(defun bar (& more) more)"], @"foo.arity:bar/n");
    XCTAssertEqualObjects([jsl rep:@"(defun foo (n) n)"], @"foo.arity:foo/1");
    XCTAssertEqualObjects([jsl rep:@"(defmodule bar.arity (export all) (import (from foo.arity (bar n))))"], @"bar.arity");
    XCTAssertEqualObjects([jsl rep:@"(defun zoo (n) (bar 1 2 n))"], @"bar.arity:zoo/1");
    XCTAssertEqualObjects([jsl rep:@"(foo.arity:foo 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(foo.arity:bar 1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(bar.arity:zoo 3)"], @"(1 2 3)");
}

- (void)testInfo {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(def a 3)"], @"3");
    [jsl rep:@"(def a-info (info a))"];
    XCTAssertEqualObjects([jsl rep:@"(:type a-info)"], @"\"number\"");
    XCTAssertEqualObjects([jsl rep:@"(:module a-info)"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(:position a-info)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(def s (symbol \"foo:bar/n\"))"], @"user:bar/n");
    [jsl rep:@"(def s-info (info s))"];
    XCTAssertEqualObjects([jsl rep:@"(:type s-info)"], @"\"symbol\"");
    XCTAssertEqualObjects([jsl rep:@"(:module s-info)"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(:initial-module s-info)"], @"\"foo\"");
    XCTAssertEqualObjects([jsl rep:@"(:qualified? s-info)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(:function? s-info)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(:arity s-info)"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(def f (fn (n) (+ n 3)))"], @"user:f/1");
    [jsl rep:@"(def f-info (info user:f/1))"];
    XCTAssertEqualObjects([jsl rep:@"(:type f-info)"], @"\"function\"");
    XCTAssertEqualObjects([jsl rep:@"(:module f-info)"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(:arity f-info)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(:macro? f-info)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(defmacro m (n) (+ n 3))"], @"user:m/1");
    [jsl rep:@"(def m-info (info user:m/1))"];
    XCTAssertEqualObjects([jsl rep:@"(:type m-info)"], @"\"function\"");
    XCTAssertEqualObjects([jsl rep:@"(:module m-info)"], @"\"user\"");
    XCTAssertEqualObjects([jsl rep:@"(:arity m-info)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(:macro? m-info)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(def z (atom {:a 4}))"], @"(atom {:a 4})");
    [jsl rep:@"(def z-info (info z))"];
    XCTAssertEqualObjects([jsl rep:@"(:type z-info)"], @"\"atom\"");
    XCTAssertEqualObjects([jsl rep:@"(:value-type z-info)"], @"\"hash-map\"");
}

- (void)testSortInternal {
    // JSList
    JSNumber *n1 = [[JSNumber alloc] initWithInteger:-1];
    JSNumber *n2 = [[JSNumber alloc] initWithInteger:7];
    JSNumber *n3 = [[JSNumber alloc] initWithInteger:3];
    JSNumber *n4 = [[JSNumber alloc] initWithInteger:2];
    JSList *list = [[JSList alloc] initWithArray:[@[n1, n2, n3, n4] mutableCopy]];
    JSList *sorted = [list sort:sortAscending];
    XCTAssertEqualObjects([sorted first], n1);
    sorted = [list sort:sortDescending];
    XCTAssertEqualObjects([sorted first], n2);
}

- (void)testSort {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(sort :asc '(3 5 2 -1 8))"], @"(-1 2 3 5 8)");
    XCTAssertEqualObjects([jsl rep:@"(sort :desc '(3 5 2 -1 8))"], @"(8 5 3 2 -1)");
    XCTAssertEqualObjects([jsl rep:@"(sort :asc [3 5 2 -1 8])"], @"[-1 2 3 5 8]");
    XCTAssertEqualObjects([jsl rep:@"(sort :desc [3 5 2 -1 8])"], @"[8 5 3 2 -1]");
    XCTAssertEqualObjects([jsl rep:@"(sort :asc \"We are Legends\")"], @"\"  LWadeeeegnrs\"");
    XCTAssertEqualObjects([jsl rep:@"(sort :desc [\"We\" \"are\" \"Legends\"])"], @"[\"Legends\" \"are\" \"We\"]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:value (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1))] {:x \"We\" :y \"are\" :z \"Legends\"})"],
                          @"[\"Legends\" \"We\" \"are\"]");
    XCTAssertEqualObjects([jsl rep:@"(sort (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1)) [\"We\" \"are\" \"Legends\"])"], @"[\"Legends\" \"We\" \"are\"]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:key :asc] {:z 2 :a 4 :p -5})"], @"[:a :p :z]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:key :desc] {:z 2 :a 4 :p -5})"], @"[:z :p :a]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:value :asc] {:z 2 :a 4 :p -5})"], @"[-5 2 4]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:value :desc] {:z 2 :a 4 :p -5})"], @"[4 2 -5]");
    XCTAssertEqualObjects([jsl rep:@"(sort :asc [3 5 2 (atom -1) -1 8])"], @"[(atom -1) -1 2 3 5 8]");
    // Implicit sort
    XCTAssertEqualObjects([jsl rep:@"(sort {:a 3 :s 4 :g 5})"], @"[:a :g :s]");
    XCTAssertEqualObjects([jsl rep:@"(sort '(3 5 2 -1 8))"], @"(-1 2 3 5 8)");
    XCTAssertEqualObjects([jsl rep:@"(sort [1 6 2 7 4])"], @"[1 2 4 6 7]");
}

- (void)testFilter {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def hm1 (filter (fn (k v) (= (mod v 2) 0)) {:a 1 :b 2 :c 3 :d 4}))"];
    [jsl rep:@"(def hm2 (filter (fn (k v) (= (mod k 2) 0)) {1 10 2 20 3 30 4 40}))"];
    XCTAssertEqualObjects([jsl rep:@"(sort [:key :asc] hm1)"], @"[:b :d]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:value :asc] hm1)"], @"[2 4]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:key :asc] hm2)"], @"[2 4]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:value :asc] hm2)"], @"[20 40]");
    XCTAssertEqualObjects([jsl rep:@"(filter (fn (x) (> x 0)) [-1 4 0 -4 -5 2])"], @"[4 2]");
    XCTAssertEqualObjects([jsl rep:@"(filter (fn (x) (<= x 0)) '(-1 4 0 -4 -5 2))"], @"(-1 0 -4 -5)");
    [jsl rep:@"(def str-xs [\"It\" \"is\" \"an\" \"ancient\" \"Mariner\" \"And\" \"he\" \"stoppeth\" \"one\" \"of\" \"three\"])"];
    XCTAssertEqualObjects([jsl rep:@"(filter (fn (x) (= (count x) 3)) str-xs)"], @"[\"And\" \"one\"]");
}

- (void)testPartition {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def hmv1 (partition (fn (k v) (= (mod v 2) 0)) {:a 1 :b 2 :c 3 :d 4}))"];
    [jsl rep:@"(def hmv2 (partition (fn (k v) (= (mod k 2) 0)) {1 10 2 20 3 30 4 40}))"];
    XCTAssertEqualObjects([jsl rep:@"(count (first hmv1))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(count (second hmv1))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(sort [:key :asc] (first hmv1))"], @"[:b :d]");
    XCTAssertEqualObjects([jsl rep:@"(sort [:key :asc] (second hmv1))"], @"[:a :c]");
    XCTAssertEqualObjects([jsl rep:@"(partition (fn (x) (> x 4)) [4 2 5 3 1])"], @"[[5] [4 2 3 1]]");
}

- (void)testFlatten {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(doall (flatten [[1] [2 [3]] [[[4]]] [5]]))"], @"[1 2 3 4 5]");
    XCTAssertEqualObjects([jsl rep:@"(doall (flatten '('(1) '(2 '(3)) '('('(4))) '(5))))"], @"(1 2 3 4 5)");
    XCTAssertEqualObjects([jsl rep:@"(doall (flatten []))"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(doall (flatten '()))"], @"()");
    [jsl rep:@"(def hm (flatten {:a 1 :b {:c 2 :d 3} :e 4}))"];
    XCTAssertEqualObjects([jsl rep:@"(contains? :a hm)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? :b hm)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(contains? :c hm)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? :d hm)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? :e hm)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(count hm)"], @"4");
    XCTAssertThrows([jsl rep:@"(flatten nil)"]);
}

- (void)testJoin {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(join \"->\" \"abcd\")"], @"\"a->b->c->d\"");
    XCTAssertEqualObjects([jsl rep:@"(join \"->\" [1 2 3])"], @"[1 \"->\" 2 \"->\" 3]");
    XCTAssertEqualObjects([jsl rep:@"(join 0 '(1 2 3))"], @"(1 0 2 0 3)");
    XCTAssertEqualObjects([jsl rep:@"(join nil '(1 2 3))"], @"(1 nil 2 nil 3)");
    XCTAssertEqualObjects([jsl rep:@"(join {:b 4} [1 2 3])"], @"[1 [[:b 4]] 2 [[:b 4]] 3]");
    XCTAssertEqualObjects([jsl rep:@"(sort (join {:b 4} {:a 2 :x 5}))"], @"[[[:b 4]] [:a 2] [:x 5]]");
    XCTAssertThrows([jsl rep:@"(join 0 1)"]);
}

- (void)testZip {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(zip '(1 2) '(4 2) [7 8])"], @"((1 4 7) (2 2 8))");
    XCTAssertEqualObjects([jsl rep:@"(zip \"abc\" \"xyz\")"], @"[\"ax\" \"by\" \"cz\"]");
    XCTAssertEqualObjects([jsl rep:@"(zip [1 3] [2 0] [1 -1])"], @"[[1 2 1] [3 0 -1]]");
    XCTAssertEqualObjects([jsl rep:@"(zip [1 2 3 4] [5 6 7 8])"], @"[[1 5] [2 6] [3 7] [4 8]]");
    XCTAssertEqualObjects([jsl rep:@"(zip [1 2] \"ab\")"], @"[[1 \"a\"] [2 \"b\"]]");
    [jsl rep:@"(def hm {:a 1 :b 2})"];
    [jsl rep:@"(def ret (zip {:a 1 :b 2} {:x 3 :y 4}))"];
    XCTAssertEqualObjects([jsl rep:@"(count (first ret))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(count (second ret))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(zip '(3) [1])"], @"((3 1))");
    XCTAssertEqualObjects([jsl rep:@"(zip {:a 1} [1])"], @"[[[:a 1] 1]]");
    XCTAssertEqualObjects([jsl rep:@"(zip \"abc\" [1 2 3])"], @"[\"a1\" \"b2\" \"c3\"]");
}

- (void)testInto {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // list
    XCTAssertEqualObjects([jsl rep:@"(into '() nil)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(into '() '(1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(into '(1 2) '(3 4))"], @"(1 2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(into '(1 2) \"a\")"], @"(1 2 \"a\")");
    XCTAssertEqualObjects([jsl rep:@"(into '(1 2) \"abc\")"], @"(1 2 \"abc\")");
    XCTAssertEqualObjects([jsl rep:@"(into '() {:a 1})"], @"((:a 1))");
    XCTAssertEqualObjects([jsl rep:@"(into '(1 2) {:a 1})"], @"(1 2 (:a 1))");
    XCTAssertThrows([jsl rep:@"(into '(1 2) 1)"]);
    // vector
    XCTAssertEqualObjects([jsl rep:@"(into [] nil)"], @"[]");
    XCTAssertEqualObjects([jsl rep:@"(into [] '(1 2 3))"], @"[1 2 3]");
    XCTAssertEqualObjects([jsl rep:@"(into [1 2] '(3 4))"], @"[1 2 3 4]");
    XCTAssertEqualObjects([jsl rep:@"(into [1 2] \"a\")"], @"[1 2 \"a\"]");
    XCTAssertEqualObjects([jsl rep:@"(into [1 2] \"abc\")"], @"[1 2 \"abc\"]");
    XCTAssertEqualObjects([jsl rep:@"(into [] {:a 1})"], @"[[:a 1]]");
    XCTAssertEqualObjects([jsl rep:@"(into [1 2] {:a 1})"], @"[1 2 [:a 1]]");
    XCTAssertThrows([jsl rep:@"(into [1 2] 1)"]);
    // hash-map
    XCTAssertEqualObjects([jsl rep:@"(into {} nil)"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(into {} [:a 1])"], @"{:a 1}");
    XCTAssertEqualObjects([jsl rep:@"(count (into {:a 1} [:b 2]))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(into {} '(1 2))"], @"{1 2}");
    XCTAssertEqualObjects([jsl rep:@"(into {} {:a 1})"], @"{:a 1}");
    XCTAssertEqualObjects([jsl rep:@"(count (into {:a 1} {:b 2}))"], @"2");
    XCTAssertThrows([jsl rep:@"(into {} 1)"]);
}

- (void)testIndexOf {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    //list
    XCTAssertEqualObjects([jsl rep:@"(index-of 1 '(0 1 2))"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(index-of -1 '(0 1 -1 4 -5))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(index-of 1 '())"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(index-of nil '())"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(index-of nil '(nil))"], @"0");
    // vector
    XCTAssertEqualObjects([jsl rep:@"(index-of 1 [0 1 2])"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(index-of \"abc\" '(\"ab\" \"z\" \"abc\"))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(index-of {:a 1} [{:b 2} {:c 3} {:a 1}])"], @"2");
    // string
    XCTAssertEqualObjects([jsl rep:@"(index-of \"a\" \"abc\")"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(index-of \"abc\" \"fooabc\")"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(index-of \"a\" \"zbc\")"], @"-1");
    XCTAssertEqualObjects([jsl rep:@"(index-of \"a\" \"bc ef ab ba\")"], @"6");
    XCTAssertThrows([jsl rep:@"(index-of 1 {})"]);
    XCTAssertThrows([jsl rep:@"(index-of 1 1)"]);
    XCTAssertThrows([jsl rep:@"(index-of 1 :a)"]);
}

- (void)testMap {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // single arity function
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a) (+ a 2)) '(1 2 3 4))"], @"(3 4 5 6)");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a) (+ a 2)) [1 2 3 4])"], @"[3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a) (str a \"ok\")) \"abcd\")"], @"[\"aok\" \"bok\" \"cok\" \"dok\"]");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a) a) {:a 1})"], @"[[:a 1]]");
    // multi arity function
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) [a b]) [1 2] '(3 4))"], @"([1 3] [2 4])");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) (list a b)) [1 2] '(3 4))"], @"((1 3) (2 4))");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) (list a b)) [1 2] '[3 4])"], @"[(1 3) (2 4)]");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) (+ a b)) [1 2] '(3 4))"], @"(4 6)");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) (+ a b)) [1 2] '[3 4])"], @"[4 6]");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) (str a b)) \"abc\" \"xyz\")"], @"[\"ax\" \"by\" \"cz\"]");
    XCTAssertEqualObjects([jsl rep:@"(map (fn (a b) [a b]) {:a 1} {:x 3})"], @"[[[:a 1] [:x 3]]]");
}

- (void)testFold {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // foldl
    XCTAssertEqualObjects([jsl rep:@"(foldl (fn (x acc) (+ x acc)) 0 [1 2 3])"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(foldl (fn (x acc) (+ x acc)) 10 [])"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(foldl (fn (x acc) [(+ x (first acc))]) [0] [1 2 3])"], @"[6]");
    XCTAssertEqualObjects([jsl rep:@"(foldl (fn (x acc) (str acc x)) \"\" [\"a\" 1 \"b\" 2 \"c\" 3])"], @"\"a1b2c3\"");
    // foldr
    XCTAssertEqualObjects([jsl rep:@"(foldr (fn (x acc) (+ x acc)) 0 [1 2 3])"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(foldr (fn (x acc) (+ x acc)) 10 [])"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(foldr (fn (x acc) [(+ x (first acc))]) [0] [1 2 3])"], @"[6]");
    XCTAssertEqualObjects([jsl rep:@"(foldr (fn (x acc) (str acc x)) \"\" [\"a\" 1 \"b\" 2 \"c\" 3])"], @"\"3c2b1a\"");
    // hash-map
    [jsl rep:@"(def hm {:a 1 :b 2 :c 3})"];
    [jsl rep:@"(def hm-ret (foldr (fn (x y acc) (println x y acc) (assoc acc x y)) {} hm))"];
    XCTAssertEqualObjects([jsl rep:@"(= hm hm-ret)"], @"true");
}

- (void)testThreadingMacro {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // thread first ->
    XCTAssertEqualObjects([jsl rep:@"(-> [1 2 3] (count))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(-> \"abc\" (zip) (rest) (first))"], @"\"b\"");
    XCTAssertEqualObjects([jsl rep:@"(-> \"abc\" (concat \"xyz\"))"], @"\"abcxyz\"");
    XCTAssertEqualObjects([jsl rep:@"(-> [1 4] (zip [2 3]))"], @"[[1 2] [4 3]]");
    XCTAssertEqualObjects([jsl rep:@"(-> [1 4] (zip [2 3]) (flatten) (doall))"], @"[1 2 4 3]");
    // thread last <-
    XCTAssertEqualObjects([jsl rep:@"(<- \"abc\" (concat \"xyz\"))"], @"\"xyzabc\"");
    XCTAssertEqualObjects([jsl rep:@"(<- [1 4] (zip [2 3]))"], @"[[2 1] [3 4]]");
    XCTAssertEqualObjects([jsl rep:@"(<- [1 4] (zip [2 3]) (flatten) (doall))"], @"[2 1 3 4]");
}

- (void)testCoreLibFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // inc
    XCTAssertEqualObjects([jsl rep:@"(inc 1)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(inc 1.1)"], @"2.1");
    XCTAssertEqualObjects([jsl rep:@"(inc -1.0)"], @"0.0");
    // dec
    XCTAssertEqualObjects([jsl rep:@"(dec 1)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(dec 1.1)"], @"0.1");
    XCTAssertEqualObjects([jsl rep:@"(dec -1.0)"], @"-2.0");
    // identity
    XCTAssertEqualObjects([jsl rep:@"(identity -1.0)"], @"-1.0");
    XCTAssertEqualObjects([jsl rep:@"(identity [1 2 3])"], @"[1 2 3]");
}

- (void)testLazySequenceCoreFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // lazy-seq
    [jsl rep:@"(def lseq (lazy-seq [1 2 3]))"];
    XCTAssertEqualObjects([jsl rep:@"(type lseq)"], @"\"lazy-sequence\"");
    XCTAssertEqualObjects([jsl rep:@"(type (lazy-seq '(1 2 3)))"], @"\"lazy-sequence\"");
    XCTAssertEqualObjects([jsl rep:@"(type (lazy-seq \"abc\"))"], @"\"lazy-sequence\"");
    XCTAssertThrows([jsl rep:@"(lazy-seq 1)"]);
    // has-next?, next
    XCTAssertEqualObjects([jsl rep:@"(has-next? lseq)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(next lseq)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(next lseq)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(next lseq)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(has-next? lseq)"], @"false");
    XCTAssertThrows([jsl rep:@"(next lseq)"]);
    // flatten, doall
    XCTAssertEqualObjects([jsl rep:@"(type (flatten [1 [2 4] 3]))"], @"\"lazy-sequence\"");
    XCTAssertEqualObjects([jsl rep:@"(doall (flatten [1 [2 4] 3]))"], @"[1 2 4 3]");
    XCTAssertEqualObjects([jsl rep:@"(doall [1 [2 4] 3])"], @"[1 [2 4] 3]");
}

@end
