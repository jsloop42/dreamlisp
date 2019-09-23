//
//  DreamLispTests.m
//  DreamLispTests
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <Foundation/Foundation.h>
#import <XCTest/XCTest.h>
#import <DreamLisp/DreamLispLib.h>
#import "DLMockStdIOService.h"

@interface DLTests : XCTestCase
@end

@implementation DLTests

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
    DLString* str = [[DLString alloc] initWithString:@"Foo"];
    XCTAssertEqualObjects([str dataType], @"DLString");
}

- (void)testDLList {
    DLList* list = [DLList new];
    DLString* str = [[DLString alloc] initWithString:@"Foo"];
    DLString* str1 = str;
    [list add:str];
    [list add:str1];
    XCTAssertEqualObjects([(DLString*)[list first] value], @"Foo");
    XCTAssertEqual([list count], 2);
    list = [[DLList alloc] initWithArray: [@[[[DLString alloc] initWithString:@"1"], [[DLString alloc] initWithString:@"2"],
                                             [[DLString alloc] initWithString:@"3"], [[DLString alloc] initWithString:@"4"]] mutableCopy]];

    XCTAssertEqualObjects([(DLString *)[[list reverse] first] value], @"4");
    XCTAssertEqualObjects([(DLString *)[list first] value], @"1");
}

- (void)testJSNumber {
    DLNumber* n1 = [[DLNumber alloc] initWithDouble:3.14];
    DLNumber* n2 = n1;
    XCTAssertTrue([n1 isEqual:n2]);
}

- (void)testDLString {
    NSString *s1 = @"1";
    NSString *s2 = s1;
    s1 = @"2";
    XCTAssertEqualObjects(s2, @"1");
    DLString *s3 = [[DLString alloc] initWithFormat:@"%d", 42];
    XCTAssertFalse([s3 isMutable]);
    XCTAssertEqualObjects([s3 value], @"42");
    // Mutable test
    NSMutableString *str = [NSMutableString new];
    [str appendString:@"abc"];
    DLString *mstr = [[DLString alloc] initWithMutableString:str];
    XCTAssertTrue([mstr isMutable]);
    [mstr appendString:@"d"];
    XCTAssertEqualObjects([mstr mutableValue], @"abcd");
    // reverse
    XCTAssertEqualObjects([[[DLString alloc] initWithString:@"abcd"] reverse], @"dcba");
    XCTAssertEqualObjects([[[DLString alloc] initWithString:@"abcdefg"] reverse], @"gfedcba");
    XCTAssertEqualObjects([[[DLString alloc] initWithString:@"a"] reverse], @"a");
    XCTAssertEqualObjects([[[DLString alloc] initWithString:@""] reverse], @"");
    XCTAssertEqualObjects([[[DLString alloc] initWithString:@"1234"] reverse], @"4321");
    XCTAssertEqualObjects([[[DLString alloc] initWithString:@"[1 2 3 4]"] reverse], @"]4 3 2 1[");
    // joined
    DLString *str1 = [[DLString alloc] initWithString:@"a"];
    DLString *str2 = [[DLString alloc] initWithString:@"b"];
    DLString *str3 = [[DLString alloc] initWithString:@"c"];
    DLString *str4 = [[DLString alloc] initWithMutableString];
    NSArray<DLString *> *arr = @[str1, str2, str3];
    XCTAssertEqualObjects([[str4 joined:arr with:@", "] value], @"a, b, c");
}

- (void)testDLStringSubString {
    DLString *str = [[DLString alloc] initWithString:@"abcdefg"];
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
    DLKeyword *kwd = [[DLKeyword alloc] initWithString:@"foo"];
    XCTAssertEqualObjects([kwd value], @":foo");
    kwd = [[DLKeyword alloc] initWithKeyword:@":foo"];
    XCTAssertEqualObjects([kwd value], @":foo");
    kwd = [[DLKeyword alloc] initWithKeyword:@"foo"];
    XCTAssertEqualObjects([kwd string], @"foo");
    kwd = [[DLKeyword alloc] initWithKeyword:@":abc"];
    XCTAssertEqualObjects([kwd encoded], @"\u029e:abc");
    kwd = [[DLKeyword alloc] initWithEncodedKeyword:@"\u029e:abc"];
    XCTAssertEqualObjects([kwd value], @":abc");
    XCTAssertFalse([DLKeyword isKeyword:[[DLNumber alloc] initWithInt:1]]);
    XCTAssertFalse([DLKeyword isKeyword:[DLString new]]);
}

- (void)testJSSymbol {
    DLSymbol *sym = [[DLSymbol alloc] initWithName:@"count" moduleName:[DLConst coreModuleName]]; // `(core:count a [1])
    [sym setArity:-2];
    [sym resetArity];
    [sym setModuleName:DLConst.defaultModuleName];
    [sym setPosition:0];
    [sym setIsQualified:YES];
    XCTAssertEqualObjects([sym string], @"core:count");
}

- (void)testJSSymbolComparison {
    DLSymbol *sym1 = [[DLSymbol alloc] initWithName:@"a" moduleName:[DLState.shared currentModuleName]];
    [sym1 setArity:-2];
    DLSymbol *sym2 = [[DLSymbol alloc] initWithName:@"b" moduleName:[DLState.shared currentModuleName]];
    [sym2 setArity:-1];
    DLSymbol *sym3 = [[DLSymbol alloc] initWithName:@"c" moduleName:[DLState.shared currentModuleName]];
    [sym3 setArity:0];
    DLSymbol *sym4 = [[DLSymbol alloc] initWithName:@"d" moduleName:[DLState.shared currentModuleName]];
    [sym4 setArity:1];
    NSMutableArray *arr = [@[sym3, sym4, sym2, sym1] mutableCopy];
    [arr sortUsingComparator:^NSComparisonResult(id  _Nonnull obj1, id  _Nonnull obj2) {
        return [DLSymbol compareSymbol:obj1 withSymbol:obj2];
    }];
    XCTAssertEqualObjects([arr first], sym2);
    XCTAssertEqualObjects([arr second], sym4);
    XCTAssertEqualObjects([arr last], sym1);
}

- (void)testJSSymbolProcess {
    [DLState.shared setCurrentModuleName:DLConst.defaultModuleName];
    DLSymbol *sym = [DLSymbol processName:@"mod:func/1"];
    XCTAssertEqualObjects([sym moduleName], [DLState.shared currentModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], @"mod");
    XCTAssertEqual([sym arity], 1);
    XCTAssertEqual([sym initialArity], 1);
    XCTAssertTrue([sym isQualified]);
    XCTAssertTrue([sym isFunction]);
    sym = [DLSymbol processName:@"func/1"];
    XCTAssertEqualObjects([sym moduleName], [DLState.shared currentModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], [DLState.shared currentModuleName]);
    XCTAssertEqual([sym arity], 1);
    XCTAssertEqual([sym initialArity], 1);
    XCTAssertFalse([sym isQualified]);
    XCTAssertTrue([sym isFunction]);
    sym = [DLSymbol processName:@"var"];
    XCTAssertEqualObjects([sym moduleName], [DLState.shared currentModuleName]);
    XCTAssertEqualObjects([sym initialModuleName], [DLState.shared currentModuleName]);
    XCTAssertEqual([sym arity], -2);
    XCTAssertEqual([sym initialArity], -2);
    XCTAssertFalse([sym isQualified]);
    XCTAssertFalse([sym isFunction]);
}

- (void)testTokenize {
    DLReader *reader = [DLReader new];
    NSString *exp = @"(+ 1 2)";
    NSArray *tokens = [reader tokenize:exp];
    NSMutableArray *tokensArr = [[NSMutableArray alloc] initWithObjects: @"(", @"+", @"1", @"2", @")", nil];
    XCTAssertEqualObjects(tokens, tokensArr);
}

- (void)testMapOnDLList {
    DLList *list = [[DLList alloc] initWithArray:@[@"abc", @"def", @"ghi"]];
    NSMutableArray *uc = [list map:^NSString * (NSString *arg) {
        return [arg uppercaseString];
    }];
    XCTAssertEqualObjects(uc[0], @"ABC");
}

- (void)testMapOnDLVector {
    DLVector *vec = [[DLVector alloc] initWithArray:@[@"abc", @"def", @"ghi"]];
    NSMutableArray *uc = [vec map:^NSString * (NSString *arg) {
        return [arg uppercaseString];
    }];
    XCTAssertEqualObjects(uc[0], @"ABC");
}

- (void)testReader {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLReader *reader = [dl reader];
    NSMutableArray<id<DLDataProtocol>> *ast = [reader readString:@"(def a 1)"];
    DLList *xs = (DLList *)ast[0];
    XCTAssertEqualObjects([(DLSymbol *)[xs first] moduleName], DLConst.defaultModuleName);
    XCTAssertEqualObjects([(DLSymbol *)[xs second] moduleName], DLConst.defaultModuleName);
    XCTAssertEqualObjects([reader moduleName], DLConst.defaultModuleName);
    ast = [reader readString:@"(defmodule foo (export all))"];
    XCTAssertEqualObjects([reader moduleName], @"foo");
    ast = [reader readString:@"(def a (fn (n) (+ n 1)))"];
    xs = (DLList *)ast[0];
    XCTAssertEqualObjects([(DLSymbol *)[xs second] moduleName], @"foo");
    xs = [xs nth:2];
    XCTAssertEqualObjects([(DLSymbol *)[(DLList *)[xs nth:1] first] moduleName], @"foo");
    xs = [xs nth:2];
    XCTAssertEqualObjects([(DLSymbol *)[xs nth:1] moduleName], @"foo");
    ast = [reader readString:@"(in-module \"user\")"];
    XCTAssertEqualObjects([reader moduleName], @"foo");
    [dl rep:@"(in-module \"user\")"];
    XCTAssertEqualObjects([reader moduleName], DLConst.defaultModuleName);
    ast = [reader readString:@"(def b 2)"];
    xs = (DLList *)ast[0];
    XCTAssertEqualObjects([(DLSymbol *)[xs second] moduleName], DLConst.defaultModuleName);
    ast = [reader readString:@"(core:empty? [1])"];
    xs = (DLList *)ast[0];
    DLSymbol *sym = (DLSymbol *)[xs first];
    XCTAssertEqualObjects([sym moduleName], DLConst.defaultModuleName);
    XCTAssertEqualObjects([sym initialModuleName], DLConst.coreModuleName);
    XCTAssertTrue([sym isQualified]);
}

- (void)testPrintString {
    DLPrinter *prn = [DLPrinter new];
    // Function
    DLFunction *fn = [[DLFunction alloc] initWithAst:[DLNil new] params:[NSMutableArray new]
                      env:[DLEnv new] macro:false meta:[DLNil new] fn:^id(id arg) { return nil; } name:@"nil-fn/0"];
    XCTAssertEqualObjects([prn printStringFor:fn readably:true], @"nil-fn/0");
    // Symbol
    DLSymbol *sym = [[DLSymbol alloc] initWithName:@"greet"];
    [sym setModuleName:DLConst.defaultModuleName];
    XCTAssertEqualObjects([prn printStringFor:sym readably:true], @"user:greet");
    // Integer
    DLNumber *num = [[DLNumber alloc] initWithString:@"42"];
    XCTAssertEqual([num integerValue], 42);
    XCTAssertEqualObjects([prn printStringFor:num readably:true], @"42");
    // Double
    DLNumber *num1 = [[DLNumber alloc] initWithString:@"42.42"];
    XCTAssertTrue([num1 isDouble]);
    XCTAssertEqual([num1 doubleValue], 42.42);
    XCTAssertEqualObjects([prn printStringFor:num1 readably:true], @"42.42");
    // List with numbers
    DLList *nlist = [[DLList alloc] initWithArray:@[@1, @2, @3]];
    XCTAssertEqualObjects([prn printStringFor:nlist readably:true], @"(1 2 3)");
    // List with strings
    DLList *slist = [[DLList alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqualObjects([prn printStringFor:slist readably:true], @"(\"1\" \"2\" \"3\")");
    // Keyword
    DLKeyword *kw = [[DLKeyword alloc] initWithKeyword:@":abc"];
    XCTAssertEqualObjects([prn printStringFor:kw readably:true], @":abc");
    // Vector with numbers
    DLVector *nvec = [[DLVector alloc] initWithArray:@[@1, @2, @3]];
    XCTAssertEqualObjects([prn printStringFor:nvec readably:true], @"[1 2 3]");
    // Vector with strings
    DLVector *svec = [[DLVector alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqualObjects([prn printStringFor:svec readably:true], @"[\"1\" \"2\" \"3\"]");
}

- (void)testReadPrint {
    DLReader *reader = [DLReader new];
    DLPrinter *printer = [DLPrinter new];
    NSString *(^print)(NSString *) = ^NSString *(NSString *exp) {
        return [printer printStringFor:[reader readString:exp][0] readably:true];
    };
    XCTAssertEqualObjects(print(@"(1 2 3)"), @"(1 2 3)");
    XCTAssertEqualObjects(print(@"[1 2 3]"), @"[1 2 3]");
    XCTAssertEqualObjects(print(@"(\"1\" \"2\" \"3\")"), @"(\"1\" \"2\" \"3\")");
}

- (void)testFileOps {
    NSString *filePath = @"/tmp/dl-fileops-test.txt";
    NSString *content = @"You are my dream come true.\n";
    NSString *appendContent = @"Take my hand and let's fly\n";
    DLFileOps *readIO = [DLFileOps new];
    DLFileOps *writeIO = [DLFileOps new];
    DLFileOps *appendIO = [DLFileOps new];
    XCTAssertFalse([writeIO isFileExists:filePath]);
    XCTAssertNoThrow([writeIO createFileIfNotExist:filePath]);
    XCTAssertNoThrow([writeIO openFileForWriting:filePath]);
    [writeIO writeString:content];
    [writeIO closeFile];
    XCTAssertNoThrow([readIO openFileForReading:filePath]);
    XCTAssertTrue([readIO hasNext]);
    XCTAssertEqualObjects([readIO readLine], [content substringToIndex:[content length] - 1]);
    XCTAssertNoThrow([appendIO openFileForAppending:filePath]);
    [appendIO append:appendContent];
    [appendIO closeFile];
    // TODO: update this
    //XCTAssertTrue([readIO hasNext]);
    //XCTAssertEqualObjects([readIO readLine], [appendContent substringToIndex:[appendContent length] - 1]);
    [readIO closeFile];
    [readIO delete:filePath];
    XCTAssertFalse([writeIO isFileExists:filePath]);
}

- (void)testDLHashMap {
    DLReader *reader = [DLReader new];
    DLPrinter *printer = [DLPrinter new];
    NSString *exp = @"{\"a\" \"abc\" \"b\" \"bcd\" \"c\" \"cde\"}";
    NSArray *inp = @[@"a", @"abc", @"b", @"bcd", @"c", @"cde"];
    id<DLDataProtocol> data = [reader readString:exp][0];
    NSString *ret = [printer printStringFor:data readably:false];
    NSArray *arr = [[[ret stringByReplacingOccurrencesOfString:@"{" withString:@""] stringByReplacingOccurrencesOfString:@"}" withString:@""]
                    componentsSeparatedByString:@" "];
    XCTAssertEqual([arr count], [inp count]);
    for (int i = 0; i < [arr count]; i++) {
        XCTAssertTrue([inp containsObject:arr[i]]);
    }
    DLHashMap *dict = [DLHashMap new];
    NSString *key = @"sym";
    DLString *object = [[DLString alloc] initWithString:@"1234"];
    [dict setObject:object forKey:key];
    XCTAssertEqualObjects((DLString *)[dict objectForKey:key], object);
    // testing hash map with object keys (should implement to `isEqual`, `hash` functions).
    NSMutableDictionary *aDict = [NSMutableDictionary new];
    DLNumber *aVal = [[DLNumber alloc] initWithInt:1];
    DLNumber *aKey = [[DLNumber alloc] initWithInt:2];
    [aDict setObject:aVal forKey:aKey];
    id<DLDataProtocol> aRet = [aDict objectForKey:aKey];
    NSArray *aKeys = [aDict allKeys];
    XCTAssertTrue([aKeys count] == 1);
    aRet = [aDict objectForKey:aKeys[0]];
    XCTAssertNotNil(aRet);
    XCTAssertEqualObjects([aRet dataType], @"DLNumber");
    aRet = nil;
    aKeys = nil;
    // testing hash map with number keys
    DLHashMap *hm = [[DLHashMap alloc] initWithArray:[@[aKey, aVal] mutableCopy]];
    aKeys = [hm allKeys];
    XCTAssertTrue([aKeys count] == 1);
    aRet = [hm objectForKey:aKeys[0]];
    XCTAssertNotNil(aRet);
    XCTAssertEqualObjects([aRet dataType], @"DLNumber");
}

- (void)testDLListRest {
    DLList *xs = [[DLList alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqual([xs count], 3);
    DLList *rest = (DLList *)[xs rest];
    XCTAssertEqual([xs count], 3);
    XCTAssertEqual([rest count], 2);
}

- (void)testDLListDropLast {
    DLList *xs = [[DLList alloc] initWithArray:@[@"1", @"2", @"3"]];
    XCTAssertEqual([xs count], 3);
    DLList *list = (DLList *)[xs dropLast];
    XCTAssertEqual([xs count], 3);
    XCTAssertEqual([list count], 2);
    XCTAssertEqualObjects([list last], [xs second]);
}

- (void)testLoadingDLModules {
    DreamLisp *dl = [[DreamLisp alloc] init];
    [dl bootstrap];
    [dl loadDLModuleLibs];
    XCTAssertTrue([[DLEnv modules] containsKey:[DLConst coreModuleName]]);
    XCTAssertTrue([[DLEnv modules] containsKey:[DLConst testModuleName]]);
}

- (void)testSymbol {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(symbol \"foo:bar/1\")"], @"foo:bar/1");
    XCTAssertEqualObjects([dl rep:@"(def z 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(eval (symbol \"user:z\"))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(symbol \"1\")"], @"*:1");
    XCTAssertEqualObjects([dl rep:@"(symbol \"a\")"], @"*:a");
    XCTAssertEqualObjects([dl rep:@"(symbol \"abc\")"], @"*:abc");
    XCTAssertEqualObjects([dl rep:@"(symbol \"abc/1\")"], @"*:abc/1");
    XCTAssertEqualObjects([dl rep:@"(symbol \"abc/n\")"], @"*:abc/n");
    // Test qualified symbol
    XCTAssertEqualObjects([dl rep:@"(symbol \"foo:1\")"], @"foo:1");
    XCTAssertEqualObjects([dl rep:@"(symbol \"foo:a\")"], @"foo:a");
    XCTAssertEqualObjects([dl rep:@"(symbol \"foo:abc\")"], @"foo:abc");
    XCTAssertEqualObjects([dl rep:@"(symbol \"foo:abc/1\")"], @"foo:abc/1");
    XCTAssertEqualObjects([dl rep:@"(symbol \"foo:abc/n\")"], @"foo:abc/n");
    // function argument
    [dl rep:@"(def a core:inc/1)"];
    XCTAssertEqualObjects([dl rep:@"(symbol a)"], @"user:a/1");
    XCTAssertEqualObjects([dl rep:@"(symbol (fn (n) 1))"], @"*:*/1");
}

- (void)testArithmeticEval {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(+ 1 2)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(+ 1 2 3)"], @"6");
    XCTAssertEqualObjects([dl rep:@"(+ 1 2 3 4)"], @"10");
    XCTAssertEqualObjects([dl rep:@"(+ 1 2 3 -4)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(- 1 2)"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(- 5 1 1)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(- 5 3 2 1)"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(- 5 4 -2 1)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(* 5 4 -2 1)"], @"-40");
    XCTAssertEqualObjects([dl rep:@"(- (- 5 4) 2)"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(+ (- (- 5 4) 2) 43)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(* (- (- 5 4) 2) -42)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(/ (+ (* (* 5 -4) -4) 4) 2)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(/ (+ (* (* 5.5 -4.20) -4.41) 4.314) 2.24)"], @"47.404017857142857142857142857142857142");
    XCTAssertEqualObjects([dl rep:@"(+ 5 (* 2 3))"], @"11");
    XCTAssertEqualObjects([dl rep:@"(- (+ 5 (* 2 3)) 3)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(/ (- (+ 5 (* 2 3)) 3) 4)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(/ (- (+ 515 (* 87 311)) 302) 27)"], @"1010");
    XCTAssertEqualObjects([dl rep:@"(* -3 6)"], @"-18");
    XCTAssertEqualObjects([dl rep:@"(/ (- (+ 515 (* -87 311)) 296) 27)"], @"-994");
    XCTAssertEqualObjects([dl rep:@"[1 2 (+ 1 2)]"], @"[1 2 3]");
    XCTAssertEqualObjects([dl rep:@"{\"a\" (+ 7 8)}"], @"{\"a\" 15}");
    XCTAssertEqualObjects([dl rep:@"{:a (+ 7 8)}"], @"{:a 15}");
    XCTAssertEqualObjects([dl rep:@"(/ 100 2 2)"], @"25");
    XCTAssertEqualObjects([dl rep:@"(/ 100 2 2 2)"], @"12.5");
    XCTAssertEqualObjects([dl rep:@"(/ 100 2 2 2 2)"], @"6.25");
    XCTAssertEqualObjects([dl rep:@"(/ 100 2 2 2 2 2)"], @"3.125");
    XCTAssertEqualObjects([dl rep:@"(/ 100 2 2 2 2 2 2)"], @"1.5625");
    XCTAssertEqualObjects([dl rep:@"(/ (- (+ 5 (* 2 3)) 3) 4)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(mod 42 21)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(mod 5 3)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(mod 5 3.5)"], @"1.5");
    XCTAssertEqualObjects([dl rep:@"(mod 5 -3.5)"], @"-2.0");
    // zero argument
    XCTAssertEqualObjects([dl rep:@"(+)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(*)"], @"1");
    // one argument
    XCTAssertEqualObjects([dl rep:@"(+ 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(+ 5)"], @"5");
    XCTAssertEqualObjects([dl rep:@"(+ 7.0)"], @"7.0");
    XCTAssertEqualObjects([dl rep:@"(+ 31.1)"], @"31.1");
    XCTAssertEqualObjects([dl rep:@"(- 1)"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(- 1.0)"], @"-1.0");
    XCTAssertEqualObjects([dl rep:@"(- 5.0)"], @"-5.0");
    XCTAssertEqualObjects([dl rep:@"(- 53.6)"], @"-53.6");
    XCTAssertEqualObjects([dl rep:@"(* 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(* 3.1415)"], @"3.1415");
    XCTAssertEqualObjects([dl rep:@"(/ 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(/ -1)"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(/ 1.0)"], @"1.0");
    XCTAssertEqualObjects([dl rep:@"(/ 2)"], @"0.5");
    XCTAssertEqualObjects([dl rep:@"(/ -2)"], @"-0.5");
    XCTAssertEqualObjects([dl rep:@"(/ 4)"], @"0.25");
}

- (void)testComparisonFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(< 42 84)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(> 42 21)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 42 42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(> 42 43)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(>= 42 43)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(<= 42 41)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 42 42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 42 21)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 42.0 42.0)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 42.42 42.42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 42.42 42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(> 42.42 42.21)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(< 42.42 42.21)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(<= 42.42 42.21)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(< 42.21 42.42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(<= 42.42 42.42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 42.42 42.42)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(> 47.404017857142857142857142857142857142 47.404017857142857142857142857142857141)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 47.404017857142857142857142857142857142 47.404017857142857142857142857142857142)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(> 2 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(> 1 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(> 1 2)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(>= 2 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 1 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 1 2)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(< 2 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(< 1 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(< 1 2)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(<= 2 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(<= 1 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(<= 1 2)"], @"true");
    // testing one argument
    XCTAssertEqualObjects([dl rep:@"(<= 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(<= 0)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 1.1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(>= 1)"], @"true");
}

- (void)testPrintFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLMockStdIOService *stdIOService = [DLMockStdIOService new];
    [[dl ioService] setStdIODelegate:stdIOService];
    XCTAssertEqualObjects([dl rep:@"(println [33 2 3])"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"[33 2 3]");
    XCTAssertEqualObjects([dl rep:@"(prn [(+ 21 12) 2 3])"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"[33 2 3]");
    XCTAssertEqualObjects([dl rep:@"(prn)"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"");
    XCTAssertEqualObjects([dl rep:@"(prn \"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(prn \"abc\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"(prn \"abc  def\" \"ghi jkl\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc  def\" \"ghi jkl\"");
    XCTAssertEqualObjects([dl rep:@"(prn \"\\\"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(prn \"abc\\ndef\\nghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc\\ndef\\nghi\"");
    XCTAssertEqualObjects([dl rep:@"(prn \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"abc\\\\\\\\def\\\\\\\\ghi\"");
    XCTAssertEqualObjects([dl rep:@"(prn (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"(1 2 \"abc\" \"\\\"\") \"def\"");
    // (println)
    XCTAssertEqualObjects([dl rep:@"(println)"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"");
    XCTAssertEqualObjects([dl rep:@"(println \"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"");
    XCTAssertEqualObjects([dl rep:@"(println \"abc\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc");
    XCTAssertEqualObjects([dl rep:@"(println \"abc  def\" \"ghi jkl\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc  def ghi jkl");
    XCTAssertEqualObjects([dl rep:@"(println \"\\\"\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"");
    XCTAssertEqualObjects([dl rep:@"(println \"abc\\ndef\\nghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc\ndef\nghi");
    XCTAssertEqualObjects([dl rep:@"(println \"abc\\\\def\\\\ghi\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"abc\\def\\ghi");
    XCTAssertEqualObjects([dl rep:@"(println (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"(1 2 abc \") def");
}

- (void)testDef {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // def with quote in symbol name
    XCTAssertEqualObjects([dl rep:@"(def x 10)"], @"10");
    XCTAssertEqualObjects([dl rep:@"x"], @"10");
    XCTAssertEqualObjects([dl rep:@"(def a' 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"a'"], @"1");
    XCTAssertEqualObjects([dl rep:@"(def b' '(11 12 13 14))"], @"(11 12 13 14)");
    XCTAssertEqualObjects([dl rep:@"b'"], @"(11 12 13 14)");
}

- (void)testList {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"()"], @"()");
    XCTAssertEqualObjects([dl rep:@"(list)"], @"()");
    XCTAssertEqualObjects([dl rep:@"(list 1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(list 1 (list 21 22 23) 3)"], @"(1 (21 22 23) 3)");
    XCTAssertEqualObjects([dl rep:@"(first nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(rest nil)"], @"()");
    XCTAssertEqualObjects([dl rep:@"(first [])"], @"nil");
}

- (void)testVector {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(if [] 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(count [1 2 3])"], @"3");
    XCTAssertEqualObjects([dl rep:@"(empty? [1 2 3])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(empty? [])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(list? [4 5 6])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [] (list))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [7 8] [7 8])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list 1 2) [1 2])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list 1) [])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [] [1])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 0 [])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [] 0)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [] \"\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"\" [])"], @"false");
    XCTAssertEqualObjects([dl rep:@"((fn [] 4) )"], @"4");
    XCTAssertEqualObjects([dl rep:@"((fn [f x] (f x)) (fn [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(= [(list)] (list []))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(vector 3 4 5)"], @"[3 4 5]");
    XCTAssertEqualObjects([dl rep:@"[:a :b :c]"], @"[:a :b :c]");
    XCTAssertEqualObjects([dl rep:@"[:a (+ 1 2) \"z\"]"], @"[:a 3 \"z\"]");
}

- (void)testHashMap {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"{\"abc\" 1}"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([dl rep:@"{\"a\" (+ 1 2)}"], @"{\"a\" 3}");
    XCTAssertEqualObjects([dl rep:@"{:a (+ 7 8)}"], @"{:a 15}");
    XCTAssertEqualObjects([dl rep:@"(dissoc {:a 1 :b 2} :a)"], @"{:b 2}");
    NSString *ret = [dl rep:@"(keys {:abc 123 :def 456})"];
    XCTAssertTrue([ret isEqual:@"(:abc :def)"] || [ret isEqual:@"(:def :abc)"]);
    XCTAssertEqualObjects([dl rep:@"(contains? :abc {:abc nil})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? :abc {:abc 123})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(get :abc {:abc 123})"], @"123");
    [dl rep:@"(def hm4 (assoc {:a 1 :b 2} :a 3 :c 1))"];
    XCTAssertEqualObjects([dl rep:@"(get :a hm4)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(get :b hm4)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(get :c hm4)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(hash-map \"a\" 1)"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(assoc {} \"a\" 1)"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(get \"a\" (assoc (assoc {\"a\" 1 } \"b\" 2) \"c\" 3))"], @"1");
    XCTAssertEqualObjects([dl rep:@"(def hm1 (hash-map))"], @"{}");
    XCTAssertEqualObjects([dl rep:@"(hash-map? hm1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(hash-map? 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(hash-map? \"abc\")"], @"false");
    XCTAssertThrows([dl rep:@"(get \"a\" nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(get \"a\" hm1)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(contains? \"a\" hm1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(def hm2 (assoc hm1 \"a\" 1))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(get \"a\" hm1)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(contains? \"a\" hm1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(get \"a\" hm2)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(contains? \"a\" hm2)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(keys hm1)"], @"()");
    XCTAssertEqualObjects([dl rep:@"(keys hm2)"], @"(\"a\")");
    XCTAssertEqualObjects([dl rep:@"(values hm1)"], @"()");
    XCTAssertEqualObjects([dl rep:@"(values hm2)"], @"(1)");
    XCTAssertEqualObjects([dl rep:@"(count (keys (assoc hm2 \"b\" 2 \"c\" 3)))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(assoc {} :bcd 234)"], @"{:bcd 234}");
    XCTAssertEqualObjects([dl rep:@"(keyword? (nth 0 (keys {:abc 123 :def 456})))"], @"true");
    [dl rep:@"(def hm3 (assoc hm2 \"b\" 2))"];
    XCTAssertEqualObjects([dl rep:@"(keyword? (nth 0 (keys {\":abc\" 123 \":def\" 456})))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(keyword? (nth 0 (values {\"a\" :abc \"b\" :def})))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(assoc {} :bcd nil)"], @"{:bcd nil}");
    // overwrite duplicate key
    XCTAssertEqualObjects([dl rep:@"(assoc {:a 1} :a 3)"], @"{:a 3}");
    // testing dissoc
    [dl rep:@"(def hm3 (assoc hm2 \"b\" 2))"];
    XCTAssertEqualObjects([dl rep:@"(count (keys hm3))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(count (values hm3))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(dissoc hm3 \"a\")"], @"{\"b\" 2}");
    XCTAssertEqualObjects([dl rep:@"(dissoc hm3 \"a\" \"b\")"], @"{}");
    XCTAssertEqualObjects([dl rep:@"(dissoc hm3 \"a\" \"b\" \"c\")"], @"{}");
    XCTAssertEqualObjects([dl rep:@"(count (keys hm3))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(count hm3)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(count {:a 1 :b 2 :c 3 :d 4})"], @"4");
    XCTAssertEqualObjects([dl rep:@"(dissoc {:cde 345 :fgh 456} :cde)"], @"{:fgh 456}");
    XCTAssertEqualObjects([dl rep:@"(dissoc {:cde nil :fgh 456} :cde)"], @"{:fgh 456}");
    XCTAssertEqualObjects([dl rep:@"(dissoc {:a 1 :b 4} :a)"], @"{:b 4}");
    // testing equality
    XCTAssertEqualObjects([dl rep:@"(= {} {})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 11 :b 22} (hash-map :b 22 :a 11))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 11 :b [22 33]} (hash-map :b [22 33] :a 11))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 11 :b {:c 33}} (hash-map :b {:c 33} :a 11))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 11 :b 22} (hash-map :b 23 :a 11))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= {:a 11 :b 22} (hash-map :a 11))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= {:a [11 22]} {:a (list 11 22)})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 11 :b 22} (list :a 11 :b 22))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= {} [])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [] {})"], @"false");
    // null key
    XCTAssertEqualObjects([dl rep:@"(def a nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(assoc {} a 2)"], @"{nil 2}");
    // any key
    XCTAssertEqualObjects([dl rep:@"{1 1}"], @"{1 1}");
    XCTAssertEqualObjects([dl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"{[\"x\" \"y\"] 1}"], @"{[\"x\" \"y\"] 1}");
    // hash map key evaluation
    XCTAssertEqualObjects([dl rep:@"(def a 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"{a 2}"], @"{1 2}");
    XCTAssertThrows([dl rep:@"{b 2}"], @"'b' not found");
    XCTAssertEqualObjects([dl rep:@"(contains? a {a 2})"], @"true");
}

/** Test hash map keyword key as get function*/
- (void)testHashMapKeywordFuncKey {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(:a {:a '(+ 1 2)})"], @"(user:+ 1 2)");
    [dl rep:@"(def khm {:a 2 :b 3 :c 5 :d [10 11 \"a\"]})"];
    XCTAssertEqualObjects([dl rep:@"(:a khm)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(:b khm)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(:c khm)"], @"5");
    XCTAssertEqualObjects([dl rep:@"(:d khm)"], @"[10 11 \"a\"]");
    XCTAssertEqualObjects([dl rep:@"(:a {:a 1})"], @"1");
    XCTAssertEqualObjects([dl rep:@"(:a (hash-map :a 1))"], @"1");
    XCTAssertEqualObjects([dl rep:@"(def tag :success)"], @":success");
    XCTAssertEqualObjects([dl rep:@"(def hm (atom {:success 0}))"], @"(atom {:success 0})");
    XCTAssertEqualObjects([dl rep:@"(reset! hm (assoc @hm tag (+ (get tag @hm) 1)))"], @"{:success 1}");
}

- (void)testEnv {
    DLEnv *env = [DLEnv new];
    [env setModuleName:DLConst.defaultModuleName];
    DLString *obj = [[DLString alloc] initWithString:@"123"];
    DLSymbol *key = [[DLSymbol alloc] initWithName:@"key"];
    [key setModuleName:DLConst.defaultModuleName];
    [env setObject:obj forKey:key];
    XCTAssertEqualObjects([env objectForKey:key], obj);
    DLEnv *aEnv = [[DLEnv alloc] initWithEnv:env];  // Nested env
    DLSymbol *aKey = [[DLSymbol alloc] initWithName:@"aKey"];
    DLString *aObj = [[DLString alloc] initWithString:@"987"];
    [aKey setModuleName:DLConst.defaultModuleName];
    [aEnv setObject:aObj forKey:aKey];
    XCTAssertEqualObjects([aEnv objectForKey:aKey], aObj);
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(list? *ARGV*)"], @"true");
    XCTAssertEqualObjects([dl rep:@"*ARGV*"], @"()");
    // Test with reader
    DLReader *reader = [DLReader new];  // In default module, user
    DLEnv *denv = [[DLEnv alloc] initWithModuleName:DLConst.defaultModuleName isUserDefined:NO];
    NSMutableArray *xs = [reader readString:@"(def a 1)"];
    DLList *list = (DLList *)[xs first];
    key = [list nth:1];
    id<DLDataProtocol> elem = [list nth:2];
    [denv setObject:elem forKey:key];
    XCTAssertEqual([[denv exportTable] count], 1);
    XCTAssertEqual([[denv exportTable] objectForSymbol:key], elem);
    [reader readString:@"(defmodule foo (export all))"];
    xs = [reader readString:@"(def b 2)"];
    list = (DLList *)[xs first];
    key = [list nth:1];
    elem = [list nth:2];
    DLEnv *fooEnv = [[DLEnv alloc] initWithModuleName:@"foo" isUserDefined:YES];
    [fooEnv setObject:elem forKey:key];
    XCTAssertEqual([[fooEnv exportTable] count], 0);
    XCTAssertEqual([[fooEnv internalTable] count], 1);
    XCTAssertEqual([[fooEnv internalTable] objectForSymbol:key], elem);
}

- (void)testSpecialForms {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // def
    XCTAssertEqualObjects([dl rep:@"(def x 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"x"], @"3");
    XCTAssertEqualObjects([dl rep:@"(def x 4)"], @"4");
    XCTAssertEqualObjects([dl rep:@"x"], @"4");
    XCTAssertEqualObjects([dl rep:@"(def y (+ 1 7))"], @"8");
    XCTAssertEqualObjects([dl rep:@"y"], @"8");
    // case sensitive symbols
    XCTAssertEqualObjects([dl rep:@"(def mynum 111)"], @"111");
    XCTAssertEqualObjects([dl rep:@"(def MYNUM 222)"], @"222");
    XCTAssertEqualObjects([dl rep:@"mynum"], @"111");
    XCTAssertEqualObjects([dl rep:@"MYNUM"], @"222");
    // env lookup error
    XCTAssertEqualObjects([dl rep:@"(try (abc 1 2 3) (catch ex (str ex)))"], @"\"'user:abc/3' not found\"");
    // error aborts def being re-set
    XCTAssertEqualObjects([dl rep:@"(def w 123)"], @"123");
    XCTAssertThrows([dl rep:@"(def w (abc))"], @"Symbol not found");
    XCTAssertEqualObjects([dl rep:@"w"], @"123");
    // let form
    XCTAssertEqualObjects([dl rep:@"(let (z (+ 2 3)) (+ 1 z))"], @"6");
    XCTAssertEqualObjects([dl rep:@"(let [z 9] z)"], @"9");
    XCTAssertEqualObjects([dl rep:@"(let (x 9) x)"], @"9");
    XCTAssertEqualObjects([dl rep:@"x"], @"4");
    XCTAssertEqualObjects([dl rep:@"(let (z (+ 2 3)) (+ 1 z))"], @"6");
    XCTAssertEqualObjects([dl rep:@"(let (p (+ 2 3) q (+ 2 p)) (+ p q))"], @"12");
    XCTAssertEqualObjects([dl rep:@"(def y (let (z 7) z))"], @"7");
    XCTAssertEqualObjects([dl rep:@"y"], @"7");
    XCTAssertEqualObjects([dl rep:@"(let (x (or nil \"yes\")) x)"], @"\"yes\"");
    // outer env
    XCTAssertEqualObjects([dl rep:@"(def a 4)"], @"4");
    XCTAssertEqualObjects([dl rep:@"(let (q 9) q)"], @"9");
    XCTAssertEqualObjects([dl rep:@"(let (q 9) a)"], @"4");
    XCTAssertEqualObjects([dl rep:@"(let (z 2) (let (q 9) a))"], @"4");
    // let with vector binding
    XCTAssertEqualObjects([dl rep:@"(let [z 9] z)"], @"9");
    XCTAssertEqualObjects([dl rep:@"(let [p (+ 2 3) q (+ 2 p)] (+ p q))"], @"12");
    // vector evaluation
    XCTAssertEqualObjects([dl rep:@"(let (a 5 b 6) [3 4 a [b 7] 8])"], @"[3 4 5 [6 7] 8]");
}

/** Test core forms with do */
- (void)testCoreForms {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // Test fn
    [dl rep:@"(def a (atom 0))"];
    XCTAssertEqualObjects([dl rep:@"(def sum (fn (n) (reset! a n) (reset! a (+ @a 1))))"], @"user:sum/1");
    XCTAssertEqualObjects([dl rep:@"(sum 5)"], @"6");
    // Test let
    XCTAssertEqualObjects([dl rep:@"(let (x 1 y 2) (reset! a x) (reset! a (+ x y)))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(let (a 11 b (fn (n) (+ n 1)) c (atom 0)) (reset! c (+ a 10)) (b @c))"], @"22");
    XCTAssertEqualObjects([dl rep:@"(try (reset! a 10) (reset! a (+ @a 2)))"], @"12");
    XCTAssertEqualObjects([dl rep:@"(try (throw \"foo\") (catch e (reset! a 1) (reset! a (+ @a 10))))"], @"11");
}

- (void)testFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"((fn (a b) (+ b a)) 3 4)"], @"7");
    XCTAssertEqualObjects([dl rep:@"((fn [f x] (f x)) (fn [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(((fn (a) (fn (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"((fn (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([dl rep:@"((fn () 4))"], @"4");
    XCTAssertEqualObjects([dl rep:@"((fn (f x) (f x)) (fn (a) (+ 1 a)) 7)"], @"8");
    // closure
    XCTAssertEqualObjects([dl rep:@"(((fn (a) (fn (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([dl rep:@"(def gen-plus5 (fn () (fn (b) (+ 5 b))))"], @"user:gen-plus5/0");
    XCTAssertEqualObjects([dl rep:@"(def plus5 (gen-plus5))"], @"user:plus5/1");
    XCTAssertEqualObjects([dl rep:@"(plus5 7)"], @"12");
    XCTAssertEqualObjects([dl rep:@"(def gen-plusX (fn (x) (fn (b) (+ x b))))"], @"user:gen-plusX/1");
    XCTAssertEqualObjects([dl rep:@"(def plus7 (gen-plusX 7))"], @"user:plus7/1");
    XCTAssertEqualObjects([dl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([dl rep:@"(def sumdown (fn (N) (if (> N 0) (+ N (sumdown  (- N 1))) 0)))"], @"user:sumdown/1");
    XCTAssertEqualObjects([dl rep:@"(sumdown 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(sumdown 2)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(sumdown 6)"], @"21");
    XCTAssertEqualObjects([dl rep:@"(def fib (fn (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"], @"user:fib/1");
    XCTAssertEqualObjects([dl rep:@"(fib 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(fib 2)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(fib 4)"], @"5");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) more) 2)"], @"(2)");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) (list? more)) 1 2 3)"], @"true");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) (count more)) 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) (count more)))"], @"0");
    XCTAssertEqualObjects([dl rep:@"((fn (& more) (list? more)))"], @"true");
    XCTAssertEqualObjects([dl rep:@"((fn (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([dl rep:@"((fn (a & more) (count more)) 1)"], @"0");
    XCTAssertEqualObjects([dl rep:@"((fn (a & more) (list? more)) 1)"], @"true");
    // Test apply
    XCTAssertEqualObjects([dl rep:@"(apply + 2 3)"], @"5");
    XCTAssertEqualObjects([dl rep:@"(apply + 4 5 10)"], @"19");
    XCTAssertEqualObjects([dl rep:@"(apply list [])"], @"()");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (a b) (+ a b)) [2 3])"], @"5");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (a b) (+ a b)) [4 5])"], @"9");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (& more) (list? more)) [1 2 3])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (& more) (list? more)) [])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (a & more) (list? more)) [1])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (& form) (count form)) [1 2])"], @"2");
    // test bindings
    [dl rep:@"(def a (fn (x) (let (y x z (* x x)) (+ y z))))"];
    XCTAssertEqualObjects([dl rep:@"(a 10)"], @"110");
    // test anonymous function print
    XCTAssertEqualObjects([dl rep:@"(fn (& more) 1)"], @"#<fn/n>");
    XCTAssertEqualObjects([dl rep:@"(fn (a) 1)"], @"#<fn/1>");
    // symbols with quote in name
    XCTAssertEqualObjects([dl rep:@"(def c' 10)"], @"10");
    XCTAssertEqualObjects([dl rep:@"(def f' (fn (x) (+ c' x)))"], @"user:f'/1");
    XCTAssertEqualObjects([dl rep:@"(f' 7)"], @"17");
    // function as argument
    [dl rep:@"(def x 3)"];
    [dl rep:@"(defun identity (x) x)"];
    XCTAssertEqualObjects([dl rep:@"(identity (fn (n) n))"], @"#<fn/1>");
    XCTAssertEqualObjects([dl rep:@"(identity (fn (x y) 1))"], @"#<fn/2>");
    XCTAssertEqualObjects([dl rep:@"(fn? (identity (fn (x y) 1)))"], @"true");
    // multi-arity
    XCTAssertEqualObjects([dl rep:@"((fn (x) (let (x (fn (a b) (+ a b))) (x 1))) (fn (y) y))"], @"1");
    XCTAssertEqualObjects([dl rep:@"((fn (x) (let (x (fn (a b) (+ a b))) (x 1 2))) (fn (y) y))"], @"3");
    XCTAssertEqualObjects([dl rep:@"((fn (x) (let (x (fn (a b) (+ a b))) (+ (x 1) (x 2 3)))) (fn (y) y))"], @"6");
}

- (void)testMultiArityFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(def a (fn () 0))"];
    [dl rep:@"(def a (fn (x) 1))"];
    XCTAssertEqualObjects([dl rep:@"(a)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(a 3)"], @"1");
    // variadic function
    XCTAssertEqualObjects([dl rep:@"(def x (fn (& more) more))"], @"user:x/n");
    XCTAssertEqualObjects([dl rep:@"x/n"], @"user:x/n");
    XCTAssertEqualObjects([dl rep:@"(first (x 1 2 3 4))"], @"1");
}

- (void)testNotFunction {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(not false)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(not nil)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(not true)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not \"a\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not 0)"], @"false");
}

- (void)testOrFunction {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(or ())"], @"()");
    XCTAssertEqualObjects([dl rep:@"(or nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(or (= 1 1))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(or (= [1] [2]) (= (+ 1 1) 2))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(or (= 1 2) (= 3 4))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(or (= 1 2) (= 3 4) (= \"a\" \"a\"))"], @"true");
}

- (void)testAndFunction {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(and ())"], @"()");
    XCTAssertEqualObjects([dl rep:@"(and nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(and (= 1 1))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(and (= [1] [2]) (= (+ 1 1) 2))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(and (= [1] [1]) (= (+ 1 2) 2))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(and (= [1] [1]) (= (+ 1 1) 2))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(and (= 1 1) (= 4 4) (= -1 -1))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(and (= '(1) '(1)) true (= 4 4) (= \"a\" \"a\"))"], @"true");
}

- (void)testWhenMacro {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(def a (atom 0))"], @"(atom 0)");
    XCTAssertEqualObjects([dl rep:@"(when (= 1 1) (reset! a 2) (reset! a (+ 2 @a)) 10)"], @"10");
    XCTAssertEqualObjects([dl rep:@"@a"], @"4");
    XCTAssertEqualObjects([dl rep:@"(when (= 2 1) 10)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(when-not (= 2 1) (reset! a 20) (reset! a (+ 20 @a)) 100)"], @"100");
    XCTAssertEqualObjects([dl rep:@"@a"], @"40");
    XCTAssertEqualObjects([dl rep:@"(when-not (= 1 1) 10)"], @"nil");
}

- (void)testString {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"\"\""], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"\"abc\""], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"\"abc  def\""], @"\"abc  def\"");
    XCTAssertEqualObjects([dl rep:@"\"\\\"\""], @"\"\\\"\"");
    XCTAssertEqualObjects([dl rep:@"\"abc\\ndef\\nghi\""], @"\"abc\\ndef\\nghi\"");
    XCTAssertEqualObjects([dl rep:@"\"abc\\\\def\\\\ghi\""], @"\"abc\\\\def\\\\ghi\"");
    XCTAssertEqualObjects([dl rep:@"\"\\\\n\""], @"\"\\\\n\"");
    XCTAssertEqualObjects([dl rep:@"(seq \"abc\")"], @"(\"a\" \"b\" \"c\")");
    XCTAssertEqualObjects([dl rep:@"(apply str (seq \"this is a test\"))"], @"\"this is a test\"");
    XCTAssertEqualObjects([dl rep:@"(seq \"\")"], @"nil");
}

- (void)testCoreFunctionsOnString {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // first
    XCTAssertEqualObjects([dl rep:@"(first \"\")"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(first \"abc\")"], @"\"a\"");
    XCTAssertEqualObjects([dl rep:@"(first \"12abc\")"], @"\"1\"");
    XCTAssertEqualObjects([dl rep:@"(type (first \"abc\"))"], @"\"string\"");
    // rest
    XCTAssertEqualObjects([dl rep:@"(rest \"\")"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(rest \"a\")"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(rest \"ab\")"], @"[\"b\"]");
    XCTAssertEqualObjects([dl rep:@"(rest \"abc\")"], @"[\"b\" \"c\"]");
    // nth
    XCTAssertEqualObjects([dl rep:@"(nth 0 \"abc\")"], @"\"a\"");
    XCTAssertEqualObjects([dl rep:@"(nth 1 \"abc\")"], @"\"b\"");
    XCTAssertEqualObjects([dl rep:@"(type (nth 0 \"abc\"))"], @"\"string\"");
    XCTAssertThrows([dl rep:@"(nth -1 \"abc\")"]);
    XCTAssertThrows([dl rep:@"(nth 3 \"abc\")"]);
    XCTAssertThrows([dl rep:@"(nth 0 \"\")"]);
    // take
    XCTAssertEqualObjects([dl rep:@"(take 1 \"abc\")"], @"\"a\"");
    XCTAssertEqualObjects([dl rep:@"(take 2 \"abc\")"], @"\"ab\"");
    XCTAssertEqualObjects([dl rep:@"(take 0 \"\")"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(type (take 1 \"abc\"))"], @"\"string\"");
    XCTAssertEqualObjects([dl rep:@"(try (take 1 \"\") (catch ex ex))"], @"\"Index 1 is out of bound for length 0\"");
    XCTAssertEqualObjects([dl rep:@"(try (take -1 \"abcd\") (catch ex ex))"], @"\"Index -1 is out of bound for length 0\"");
    XCTAssertEqualObjects([dl rep:@"(try (take 10 \"abcd\") (catch ex ex))"], @"\"Index 10 is out of bound for length 4\"");
    // last
    XCTAssertEqualObjects([dl rep:@"(last \"abc\")"], @"\"c\"");
    XCTAssertEqualObjects([dl rep:@"(last \"\")"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(last \"a\")"], @"\"a\"");
    XCTAssertEqualObjects([dl rep:@"(type (last \"abc\"))"], @"\"string\"");
    // drop
    XCTAssertEqualObjects([dl rep:@"(drop 0 \"abc\")"], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"(drop 1 \"abc\")"], @"\"bc\"");
    XCTAssertEqualObjects([dl rep:@"(drop 2 \"abc\")"], @"\"c\"");
    XCTAssertEqualObjects([dl rep:@"(drop 3 \"abc\")"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(drop 4 \"abc\")"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(drop -1 \"abc\")"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(type (drop 2 \"abc\"))"], @"\"string\"");
    // reverse
    XCTAssertEqualObjects([dl rep:@"(lazy-seq? (reverse \"abc\"))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse \"abc\"))"], @"\"cba\"");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse \"a\"))"], @"\"a\"");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse \"\"))"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse \"1234\"))"], @"\"4321\"");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse \"[1 2 3 4]\"))"], @"\"]4 3 2 1[\"");
    XCTAssertEqualObjects([dl rep:@"(type (doall (reverse \"abc\")))"], @"\"string\"");
    // nth-tail
    XCTAssertEqualObjects([dl rep:@"(nth-tail 0 0 \"abcd\")"], @"\"a\"");
    XCTAssertEqualObjects([dl rep:@"(nth-tail 0 2 \"abcd\")"], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"(nth-tail 1 2 \"abcd\")"], @"\"bc\"");
    XCTAssertEqualObjects([dl rep:@"(nth-tail 3 3 \"abcd\")"], @"\"d\"");
    XCTAssertEqualObjects([dl rep:@"(type (nth-tail 3 3 \"abcd\"))"], @"\"string\"");
    XCTAssertThrows([dl rep:@"(nth-tail -1 3 \"abcd\")"]);
    XCTAssertThrows([dl rep:@"(nth-tail 1 4 \"abcd\")"]);
    XCTAssertThrows([dl rep:@"(nth-tail 1 0 \"abcd\")"]);
    // append
    XCTAssertEqualObjects([dl rep:@"(append \"a\" 0 \"bcd\")"], @"\"abcd\"");
    XCTAssertEqualObjects([dl rep:@"(append \"a\" 1 \"bcd\")"], @"\"bacd\"");
    XCTAssertEqualObjects([dl rep:@"(append \"abc\" 0 \"d\")"], @"\"abcd\"");
    XCTAssertEqualObjects([dl rep:@"(append \"ab\" 3 \"bcd\")"], @"\"bcdab\"");
    XCTAssertEqualObjects([dl rep:@"(append 1 0 \"bcd\")"], @"\"1bcd\"");
    XCTAssertEqualObjects([dl rep:@"(append '(1 2 3) 0 \"bcd\")"], @"\"(1 2 3)bcd\"");
    XCTAssertEqualObjects([dl rep:@"(append [1 2 3] 0 \"bcd\")"], @"\"[1 2 3]bcd\"");
    XCTAssertEqualObjects([dl rep:@"(append {:a 1} 0 \"bcd\")"], @"\"{:a 1}bcd\"");
    XCTAssertEqualObjects([dl rep:@"(type (append \"a\" 0 \"bcd\"))"], @"\"string\"");
    XCTAssertThrows([dl rep:@"(append 0 4 '(1 2))"]);
    // seq
    XCTAssertEqualObjects([dl rep:@"(seq \"hello\")"], @"(\"h\" \"e\" \"l\" \"l\" \"o\")");
    XCTAssertEqualObjects([dl rep:@"(type (first (seq \"hello\")))"], @"\"string\"");
}

- (void)testPrint {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(pr-str)"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str \"\")"], @"\"\\\"\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str \"\\\"\")"], @"\"\\\"\\\\\\\"\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"\"(1 2 \\\"abc\\\" \\\"\\\\\\\"\\\") \\\"def\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str \"abc\\\\ndef\\\\nghi\")"], @"\"\\\"abc\\\\\\\\ndef\\\\\\\\nghi\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"\"\\\"abc\\\\\\\\\\\\\\\\def\\\\\\\\\\\\\\\\ghi\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str (list))"], @"\"()\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str [(+ 21 12) 2 3])"], @"\"[33 2 3]\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str [1 2 \"abc\" \"\\\"\"] \"def\")"], @"\"[1 2 \\\"abc\\\" \\\"\\\\\\\"\\\"] \\\"def\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str [])"], @"\"[]\"");
    XCTAssertEqualObjects([dl rep:@"(str [1 2 \"abc\" \"\\\"\"] \"def\")"], @"\"[1 2 abc \\\"]def\"");
    XCTAssertEqualObjects([dl rep:@"(str \"A\" {:abc \"val\"} \"Z\")"], @"\"A{:abc val}Z\"");
    XCTAssertEqualObjects([dl rep:@"(str true \".\" false \".\" nil \".\" :keyw \".\" 'symb)"], @"\"true.false.nil.:keyw.user:symb\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str \"A\" {:abc \"val\"} \"Z\")"], @"\"\\\"A\\\" {:abc \\\"val\\\"} \\\"Z\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(pr-str true \".\" false \".\" nil \".\" :keyw \".\" 'symb)"],
                          @"\"true \\\".\\\" false \\\".\\\" nil \\\".\\\" :keyw \\\".\\\" user:symb\"");
    [dl rep:@"(def s (str {:abc \"val1\" :def \"val2\"}))"];
    XCTAssertEqualObjects([dl rep:@"(or (= s \"{:abc val1 :def val2}\") (= s \"{:def val2 :abc val1}\"))"], @"true");
    NSString *ret = [dl rep:@"(def p (pr-str {:abc \"val1\" :def \"val2\"}))"];
    XCTAssertTrue([ret isEqual:@"\"{:abc \\\"val1\\\" :def \\\"val2\\\"}\""] || [ret isEqual:@"\"{:def \\\"val2\\\" :abc \\\"val1\\\"}\""]);
    XCTAssertEqualObjects([dl rep:@"(or (= p \"{:abc \\\"val1\\\" :def \\\"val2\\\"}\") (= p \"{:def \\\"val2\\\" :abc \\\"val1\\\"}\"))"], @"true");
}

- (void)testStringFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(str)"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(str \"\")"], @"\"\"");
    XCTAssertEqualObjects([dl rep:@"(str \"abc\")"], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"(str \"\\\"\")"], @"\"\\\"\"");
    XCTAssertEqualObjects([dl rep:@"(str 1 \"abc\" 3)"], @"\"1abc3\"");
    XCTAssertEqualObjects([dl rep:@"(str \"abc  def\" \"ghi jkl\")"], @"\"abc  defghi jkl\"");
    XCTAssertEqualObjects([dl rep:@"(str \"abc\\\\ndef\\\\nghi\")"], @"\"abc\\\\ndef\\\\nghi\"");
    XCTAssertEqualObjects([dl rep:@"(str \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"\"abc\\\\\\\\def\\\\\\\\ghi\"");
    XCTAssertEqualObjects([dl rep:@"(str (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"\"(1 2 abc \\\")def\"");
    XCTAssertEqualObjects([dl rep:@"(str (list))"], @"\"()\"");
    XCTAssertEqualObjects([dl rep:@"(str [(+ 21 12) 2 3])"], @"\"[33 2 3]\"");
    XCTAssertEqualObjects([dl rep:@"(str [(+ 21 12) 2 3 \"foo\"])"], @"\"[33 2 3 foo]\"");
    XCTAssertEqualObjects([dl rep:@"(str [1 2 \"abc\" \"\\\"\"] \"def\")"], @"\"[1 2 abc \\\"]def\"");
    XCTAssertEqualObjects([dl rep:@"(str [])"], @"\"[]\"");
    // testing list functions that works with string
    XCTAssertEqualObjects([dl rep:@"(empty? \"abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(empty? \"\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(count \"\")"], @"0");
    XCTAssertEqualObjects([dl rep:@"(count \"abc\")"], @"3");
}

- (void)testdoForm {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLMockStdIOService *stdIOService = [DLMockStdIOService new];
    [[dl ioService] setStdIODelegate:stdIOService];
    XCTAssertEqualObjects([dl rep:@"(do (def a 6) 7 (+ a 8))"], @"14");
    XCTAssertEqualObjects([dl rep:@"a"], @"6");
    XCTAssertEqualObjects([dl rep:@"(def DO (fn (a) 7))"], @"user:DO/1");
    XCTAssertEqualObjects([dl rep:@"(DO 3)"], @"7");
    // printing
    XCTAssertEqualObjects([dl rep:@"(do (prn \"prn output1\"))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"prn output1\"");
    XCTAssertEqualObjects([dl rep:@"(do (prn \"prn output2\") 7)"], @"7");
    XCTAssertEqualObjects([stdIOService output], @"\"prn output2\"");
    XCTAssertEqualObjects([dl rep:@"(do (prn \"prn output1\") (prn \"prn output2\") (+ 1 2))"], @"3");
    XCTAssertEqualObjects([stdIOService output], @"\"prn output2\"");
}

- (void)testEquality {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(= (list 1 2 3) (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list 1 3 2) (list 1 2 3))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= (list 1 2 3) [1 2 3])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list 1 3 2) [1 2 3])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [1 2 3] [1 2 3])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [1 3 2] [1 2 3])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= [1 2 3] (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [1 3 2] (list 1 2 3))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= {:a 1 :b 2} {:a 1 :b 2})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 1 :b 2} {:b 2 :a 1})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 1 :b [1 2 3]} {:b [1 2 3] :a 1})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {:a 1 :b [1 0 3]} {:b [1 2 3] :a 1})"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= {\"a\" 1 :b [1 2 3]} {:b [1 2 3] \"a\" 1})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {\"a\" {:aa 11} :b [1 2 3]} {:b [1 2 3] \"a\" {:aa 11}})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 2 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 1 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 1 2)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 1 (+ 1 1))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 2 (+ 1 1))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= nil 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= nil nil)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 1 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 0 0)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 1 0)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"\" \"\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= \"abc\" \"abc\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= \"abc\" \"\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"\" \"abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"abc\" \"def\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"abc\" \"ABC\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= true true)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= false false)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= nil nil)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list) (list))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list 1 2) (list 1 2))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (list 1) (list))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= (list) (list 1))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= 0 (list))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= (list) 0)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= (list) \"\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"\" (list))"], @"false");
    // test symbol equality
    XCTAssertEqualObjects([dl rep:@"(= (quote abc) (quote abc))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (quote abc) (quote abcd))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= (quote abc) \"abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"abc\" (quote abc))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"abc\" (str (quote abc)))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= \"user:abc\" (str (quote abc)))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (quote abc) nil)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= nil (quote abc))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not (= 1 1))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not (= 1 2))"], @"true");
    // test single argument
    XCTAssertEqualObjects([dl rep:@"(= 0)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 1.2)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= -1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= -1.2)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= '())"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= (atom 1))"], @"true");
    // testing muti arity
    XCTAssertEqualObjects([dl rep:@"(= 1 1 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= 1.5 1.5 1.5)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= -1 -1 -1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= -1.5 -1.5 -1.5)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= -1 -1.5 -1.5)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= -1.5 -1 -1.5)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= -1.5 -1.5 -1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= '() '() '())"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [] [] [])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {} {} {})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= '(1) '(1) '(1))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= [1] [1] [1])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= {1 2} {1 2} {1 2})"], @"true");
}

- (void)testNotEqual {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(not= [1] [1] [1])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not= 1.5 1.5 1.5)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not= [])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not= (quote abc) (quote abc))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(not= (quote abc) (quote def))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(not= [1] [3])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(not= \"abc\" \"def\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(not= {:a 1} {:a 2})"], @"true");
}

- (void)testConditional {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(if (> (count (list 1 2 3)) 3) \"yes\" \"no\")"], @"\"no\"");
    XCTAssertEqualObjects([dl rep:@"(if (> 3 3) \"yes\" \"no\")"], @"\"no\"");
    XCTAssertEqualObjects([dl rep:@"(if (>= (count (list 1 2 3)) 3) \"yes\" \"no\")"], @"\"yes\"");
    XCTAssertEqualObjects([dl rep:@"(if true 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(if false 7 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(if false 7 false)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(if true (+ 1 7) (+ 1 8))"], @"8");
    XCTAssertEqualObjects([dl rep:@"(if false (+ 1 7) (+ 1 8))"], @"9");
    XCTAssertEqualObjects([dl rep:@"(if nil 7 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(if 0 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(if \"\" 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(if (list) 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(if (list 1 2 3) 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(= (list) nil)"], @"false");
    // one-way if form
    XCTAssertEqualObjects([dl rep:@"(if false (+ 1 7))"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(if nil 8 7)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(if true (+ 1 7))"], @"8");
}

- (void)testListCoreFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(list? (list))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(empty? (list))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(empty? (list 1))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(count (list 1 2 3))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(count (list))"], @"0");
    XCTAssertEqualObjects([dl rep:@"(count nil)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(apply str (seq \"this is a test\"))"], @"\"this is a test\"");
    XCTAssertEqualObjects([dl rep:@"(cons 1 (list))"], @"(1)");
    XCTAssertEqualObjects([dl rep:@"(cons 1 (list 2))"], @"(1 2)");
    XCTAssertEqualObjects([dl rep:@"(cons 1 (list 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(cons (list 1) (list 2 3))"], @"((1) 2 3)");
    XCTAssertEqualObjects([dl rep:@"(def a (list 2 3))"], @"(2 3)");
    XCTAssertEqualObjects([dl rep:@"(cons 1 a)"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"a"], @"(2 3)");
    XCTAssertEqualObjects([dl rep:@"(cons [1] [2 3])"], @"([1] 2 3)");
    XCTAssertEqualObjects([dl rep:@"(cons 1 [2 3])"], @"(1 2 3)");
    // concat
    XCTAssertThrows([dl rep:@"(concat)"], @"()");
    XCTAssertEqualObjects([dl rep:@"(concat (list 1 2))"], @"(1 2)");
    XCTAssertEqualObjects([dl rep:@"(concat (list 1 2) (list 3 4))"], @"(1 2 3 4)");
    XCTAssertEqualObjects([dl rep:@"(concat (list 1 2) (list 3 4) (list 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertThrows([dl rep:@"(concat (concat))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(concat (list) (list))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(concat \"abc\" \"xyz\")"], @"\"abcxyz\"");
    XCTAssertEqualObjects([dl rep:@"(concat \"abc\" [1 2 3])"], @"\"abc123\"");
    XCTAssertEqualObjects([dl rep:@"(concat \"abc\" [1 2 \"xyz\"])"], @"\"abc12xyz\"");
    XCTAssertEqualObjects([dl rep:@"(concat \"abc\" '(1 2 \"xyz\"))"], @"\"abc12xyz\"");
    XCTAssertEqualObjects([dl rep:@"(concat \"a\" [1 2 \"b\"] \"c\")"], @"\"a12bc\"");
    XCTAssertEqualObjects([dl rep:@"(concat [1 2 3] [4 5 6])"], @"[1 2 3 4 5 6]");
    XCTAssertEqualObjects([dl rep:@"(concat [1 2 3] '(4 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([dl rep:@"(def a (list 1 2))"], @"(1 2)");
    XCTAssertEqualObjects([dl rep:@"(def b (list 3 4))"], @"(3 4)");
    XCTAssertEqualObjects([dl rep:@"(concat a b (list 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([dl rep:@"a"], @"(1 2)");
    XCTAssertEqualObjects([dl rep:@"b"], @"(3 4)");
    XCTAssertEqualObjects([dl rep:@"(concat [1 2] (list 3 4) [5 6])"], @"(1 2 3 4 5 6)");
    // nth
    XCTAssertEqualObjects([dl rep:@"(nth 0 (list 1))"], @"1");
    XCTAssertEqualObjects([dl rep:@"(nth 1 (list 1 2))"], @"2");
    [dl rep:@"(def x \"x\")"];
    XCTAssertNotEqualObjects(@"(def x (nth (list 1 2) 2))", @"Index out of bounds");
    XCTAssertEqualObjects([dl rep:@"x"], @"\"x\"");
    // first, rest
    XCTAssertEqualObjects([dl rep:@"(first (list))"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(first (list 6))"], @"6");
    XCTAssertEqualObjects([dl rep:@"(first (list 7 8 9))"], @"7");
    XCTAssertEqualObjects([dl rep:@"(rest (list))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(rest (list 6))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(rest (list 7 8 9))"], @"(8 9)");
    XCTAssertEqualObjects([dl rep:@"(first nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(rest nil)"], @"()");
    // or
    XCTAssertEqualObjects([dl rep:@"(or)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(or 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(or 1 2 3 4)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(or false 2)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(or false nil 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(or false nil false false nil 4)"], @"4");
    XCTAssertEqualObjects([dl rep:@"(or false nil 3 false nil 4)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(or (or false 4))"], @"4");
    // cond
    XCTAssertEqualObjects([dl rep:@"(cond)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(cond true 7)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(cond true 7 true 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(cond false 7 true 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(cond false 7 false 8 \"else\" 9)"], @"9");
    XCTAssertEqualObjects([dl rep:@"(cond false 7 (= 2 2) 8 \"else\" 9)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    // conj
    [dl rep:@"(def xs '(1 2))"];
    XCTAssertEqualObjects([dl rep:@"(conj (list) 1)"], @"(1)");
    XCTAssertEqualObjects([dl rep:@"(conj (list 1) 2)"], @"(2 1)");
    XCTAssertEqualObjects([dl rep:@"(conj (list 2 3) 4)"], @"(4 2 3)");
    XCTAssertEqualObjects([dl rep:@"(conj (list 2 3) 4 5 6)"], @"(6 5 4 2 3)");
    XCTAssertEqualObjects([dl rep:@"(conj (list 1) (list 2 3))"], @"((2 3) 1)");
    XCTAssertEqualObjects([dl rep:@"(conj '(1 2 3) 4 5 6)"], @"(6 5 4 1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(conj xs 6)"], @"(6 1 2)");
    XCTAssertEqualObjects([dl rep:@"xs"], @"(1 2)");
    XCTAssertThrows([dl rep:@"(conj 1 '(1 2))"]);
    // seq
    XCTAssertEqualObjects([dl rep:@"(seq '())"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(seq '(2 3 4))"], @"(2 3 4)");
    XCTAssertEqualObjects([dl rep:@"(seq nil)"], @"nil");
    // last
    XCTAssertEqualObjects([dl rep:@"(last [1 2 3 4])"], @"4");
    XCTAssertEqualObjects([dl rep:@"(last [1 2 3 [4]])"], @"[4]");
    XCTAssertEqualObjects([dl rep:@"(last '(1 2 3 4))"], @"4");
    XCTAssertEqualObjects([dl rep:@"(last '(1 2 [3] [4 5]))"], @"[4 5]");
    XCTAssertEqualObjects([dl rep:@"(last '())"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(last [])"], @"nil");
    XCTAssertThrows([dl rep:@"(last nil)"]);
    // drop
    XCTAssertEqualObjects([dl rep:@"(drop 1 '(1 2 3))"], @"(2 3)");
    XCTAssertEqualObjects([dl rep:@"(drop 2 '(1 2 3))"], @"(3)");
    XCTAssertEqualObjects([dl rep:@"(drop 3 '(1 2 3))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(drop 4 '(1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(drop -4 '(1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(drop -3 '(1 2 3))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(drop -2 '(1 2 3))"], @"(1)");
    XCTAssertEqualObjects([dl rep:@"(drop -1 '(1 2 3))"], @"(1 2)");
    XCTAssertEqualObjects([dl rep:@"(drop 0 '(1 2 3))"], @"(1 2 3)");
    // reverse
    XCTAssertEqualObjects([dl rep:@"(lazy-seq? (reverse '()))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse '()))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse '(1 2 3)))"], @"(3 2 1)");
    XCTAssertThrows([dl rep:@"(doall (reverse {:a 1 :b 2}))"]);
    // nth-tail
    XCTAssertEqualObjects([dl rep:@"(nth-tail 0 0 '(0))"], @"(0)");
    XCTAssertEqualObjects([dl rep:@"(nth-tail 2 4 '(0 1 2 3 4))"], @"(2 3 4)");
    XCTAssertThrows([dl rep:@"(nth-tail 5 10 '(0 1 2))"]);
    XCTAssertThrows([dl rep:@"(nth-tail -1 2 '(0 1 2))"]);
    XCTAssertThrows([dl rep:@"(nth-tail 1 3 '(0 1 2))"]);
    // take
    XCTAssertEqualObjects([dl rep:@"(take 2 '(0 1 2 3))"], @"(0 1)");
    XCTAssertEqualObjects([dl rep:@"(take 0 '(0 1 2 3))"], @"()");
    XCTAssertThrows([dl rep:@"(take -1 '(0))"]);
    XCTAssertThrows([dl rep:@"(take 2 '(0))"]);
    XCTAssertThrows([dl rep:@"(take 2 nil)"]);
    XCTAssertThrows([dl rep:@"(take 2 \"\"))"]);
    // append
    XCTAssertEqualObjects([dl rep:@"(append 0 0 '(1))"], @"(0 1)");
    XCTAssertEqualObjects([dl rep:@"(append 0 1 '(1))"], @"(1 0)");
    XCTAssertEqualObjects([dl rep:@"(append 4 4 '(0 1 2 3 5))"], @"(0 1 2 3 4 5)");
    XCTAssertThrows([dl rep:@"(append 0 -1 '(1 2))"]);
    XCTAssertThrows([dl rep:@"(append 0 4 '(1 2))"]);
}

- (void)testVectorCoreFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a) (* 2 a)) [1 2 3]))"], @"[2 4 6]");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn [& args] (list? args)) [1 2]))"], @"[true true]");
    XCTAssertEqualObjects([dl rep:@"(vector? [10 11])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(vector? '(12 13))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(vector 3 4 5)"], @"[3 4 5]");
    // conj
    [dl rep:@"(def xs [1 2])"];
    XCTAssertEqualObjects([dl rep:@"(conj [] 1)"], @"[1]");
    XCTAssertEqualObjects([dl rep:@"(conj [1] 2)"], @"[1 2]");
    XCTAssertEqualObjects([dl rep:@"(conj [2 3] 4)"], @"[2 3 4]");
    XCTAssertEqualObjects([dl rep:@"(conj [2 3] 4 5 6)"], @"[2 3 4 5 6]");
    XCTAssertEqualObjects([dl rep:@"(conj [1] [2 3])"], @"[1 [2 3]]");
    XCTAssertEqualObjects([dl rep:@"(conj [1 2 3] 4 5 6)"], @"[1 2 3 4 5 6]");
    XCTAssertEqualObjects([dl rep:@"(conj xs 6)"], @"[1 2 6]");
    XCTAssertEqualObjects([dl rep:@"xs"], @"[1 2]");
    XCTAssertThrows([dl rep:@"(conj 1 [1 2])"]);
    // nth
    XCTAssertEqualObjects([dl rep:@"(nth 0 [1])"], @"1");
    XCTAssertEqualObjects([dl rep:@"(nth 1 [1 2])"], @"2");
    XCTAssertEqualObjects([dl rep:@"(def x \"x\")"], @"\"x\"");
    XCTAssertThrows([dl rep:@"(def x (nth [1 2] 2))"], @"Index out of bounds");
    XCTAssertEqualObjects([dl rep:@"x"], @"\"x\"");
    // first, rest
    XCTAssertEqualObjects([dl rep:@"(first [])"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(first [10])"], @"10");
    XCTAssertEqualObjects([dl rep:@"(first [10 11 12])"], @"10");
    XCTAssertEqualObjects([dl rep:@"(rest [])"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(rest [10])"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(rest [10 11 12])"], @"[11 12]");
    // seq
    XCTAssertEqualObjects([dl rep:@"(seq [2 3 4])"], @"(2 3 4)");
    XCTAssertEqualObjects([dl rep:@"(seq [])"], @"nil");
    // drop
    XCTAssertEqualObjects([dl rep:@"(drop 1 [1 2 3])"], @"[2 3]");
    XCTAssertEqualObjects([dl rep:@"(drop 2 [1 2 3])"], @"[3]");
    XCTAssertEqualObjects([dl rep:@"(drop 3 [1 2 3])"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(drop 4 [1 2 3])"], @"[1 2 3]");
    XCTAssertEqualObjects([dl rep:@"(drop -4 [1 2 3])"], @"[1 2 3]");
    XCTAssertEqualObjects([dl rep:@"(drop -3 [1 2 3])"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(drop -2 [1 2 3])"], @"[1]");
    XCTAssertEqualObjects([dl rep:@"(drop -1 [1 2 3])"], @"[1 2]");
    XCTAssertEqualObjects([dl rep:@"(drop 0 [1 2 3])"], @"[1 2 3]");
    // reverse
    XCTAssertEqualObjects([dl rep:@"(doall (reverse []))"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(doall (reverse [1 2 3]))"], @"[3 2 1]");
    // nth-tail
    XCTAssertEqualObjects([dl rep:@"(nth-tail 0 0 [0])"], @"[0]");
    XCTAssertEqualObjects([dl rep:@"(nth-tail 2 4 [0 1 2 3 4])"], @"[2 3 4]");
    XCTAssertThrows([dl rep:@"(nth-tail 5 10 [0 1 2])"]);
    // take
    XCTAssertEqualObjects([dl rep:@"(take 2 [0 1 2 3])"], @"[0 1]");
    XCTAssertEqualObjects([dl rep:@"(take 0 [0 1 2 3])"], @"[]");
    XCTAssertThrows([dl rep:@"(take -1 [0])"]);
    XCTAssertThrows([dl rep:@"(take 2 [0])"]);
    XCTAssertThrows([dl rep:@"(take 2 nil)"]);
    XCTAssertThrows([dl rep:@"(take 2 \"\"))"]);
    XCTAssertEqualObjects([dl rep:@"(take 2 (reverse [0 1 2 3]))"], @"[3 2]");
    // append
    XCTAssertEqualObjects([dl rep:@"(append 0 0 [1])"], @"[0 1]");
    XCTAssertEqualObjects([dl rep:@"(append 0 1 [1])"], @"[1 0]");
    XCTAssertEqualObjects([dl rep:@"(append 4 4 [0 1 2 3 5])"], @"[0 1 2 3 4 5]");
    XCTAssertThrows([dl rep:@"(append 0 -1 [1 3])"]);
    XCTAssertThrows([dl rep:@"(append 0 4 [1 2])"]);
    XCTAssertThrows([dl rep:@"(append 0 [1 2])"]);
}

- (void)testKeyword {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(= :abc :abc)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(= :abc :def)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(= :abc \":abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(keyword? (nth 0 (keys {:abc 123 :def 456})))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(keyword? \"abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([dl rep:@":1"], @":1");
    XCTAssertEqualObjects([dl rep:@"(keyword? :1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(def a \"abc\")"], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"(keyword a)"], @":abc");
    // testing fail conditions
    XCTAssertEqualObjects([dl rep:@"(keyword 1)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(keyword (atom 1))"], @"nil");
    XCTAssertThrows([dl rep:@"(keyword xyz)"], @"'xyz' not found");
    XCTAssertEqualObjects([dl rep:@"(keyword (atom 1))"], @"nil");
}

- (void)testQuote {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(quote 7)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(quote (1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(quote (1 2 (3 4)))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([dl rep:@"(quasiquote 7)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 2 (3 4)))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (nil))"], @"(nil)");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (unquote 7))"], @"7");
    XCTAssertEqualObjects([dl rep:@"(def a 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (unquote a))"], @"8");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 a 3))"], @"(1 user:a 3)");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 (unquote a) 3))"], @"(1 8 3)");
    XCTAssertEqualObjects([dl rep:@"(def b (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 b 3))"], @"(1 user:b 3)");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 (unquote b) 3))"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([dl rep:@"(quasiquote ((unquote 1) (unquote 2)))"], @"(1 2)");
    XCTAssertEqualObjects([dl rep:@"(def c (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 c 3))"], @"(1 user:c 3)");
    XCTAssertEqualObjects([dl rep:@"(quasiquote (1 (splice-unquote c) 3))"], @"(1 1 \"b\" \"d\" 3)");
    // testing reader macros
    XCTAssertEqualObjects([dl rep:@"'7"], @"7");
    XCTAssertEqualObjects([dl rep:@"'(1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"'(1 2 (3 4))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([dl rep:@"`7"], @"7");
    XCTAssertEqualObjects([dl rep:@"`(1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"`(1 2 (3 4))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([dl rep:@"`(nil)"], @"(nil)");
    XCTAssertEqualObjects([dl rep:@"`~7"], @"7");
    XCTAssertEqualObjects([dl rep:@"(def a 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"`(1 ~a 3)"], @"(1 8 3)");
    XCTAssertEqualObjects([dl rep:@"(def b '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([dl rep:@"`(1 b 3)"], @"(1 user:b 3)");
    XCTAssertEqualObjects([dl rep:@"`(1 ~b 3)"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([dl rep:@"(def c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([dl rep:@"`(1 c 3)"], @"(1 user:c 3)");
    XCTAssertEqualObjects([dl rep:@"`(1 ~@c 3)"], @"(1 1 \"b\" \"d\" 3)");
    XCTAssertEqualObjects([dl rep:@"(def a 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"`[1 a 3]"], @"(1 user:a 3)");
    XCTAssertEqualObjects([dl rep:@"[1 a 3]"], @"[1 8 3]");
    XCTAssertEqualObjects([dl rep:@"(def c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([dl rep:@"`[1 ~@c 3]"], @"(1 1 \"b\" \"d\" 3)");
}

/** Testing macros without nesting */
- (void)testSimpleMacros {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defmacro one () 1)"];
    XCTAssertEqualObjects([dl rep:@"(one)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(macro? one/0)"], @"true");
    [dl rep:@"(defmacro two () 2)"];
    XCTAssertEqualObjects([dl rep:@"(two)"], @"2");
    [dl rep:@"(defmacro identity (x) x)"];
    XCTAssertEqualObjects([dl rep:@"(let (a 123) (identity a))"], @"123");
    [dl rep:@"(defmacro foo (& more) `(count (list ~@more)))"];
    XCTAssertEqualObjects([dl rep:@"(foo 1 2 3)"], @"3");
    // macro with hash-map
    [dl rep:@"(defmacro hm (k v) `(hash-map ~k ~v))"];
    XCTAssertEqualObjects([dl rep:@"(hm 1 '(3 4 5))"], @"{1 (3 4 5)}");
    [dl rep:@"(defmacro hm1 (k v) `(let (x ~v) (hash-map ~k (first x))))"];
    XCTAssertEqualObjects([dl rep:@"(hm1 1 '(3 4 5))"], @"{1 3}");
    [dl rep:@"(defmacro p (x) `(let (z ~x) (list z 4 5 6 7)))"];
    XCTAssertEqualObjects([dl rep:@"(p 3)"], @"(3 4 5 6 7)");
    XCTAssertEqualObjects([dl rep:@"(hm 2 (p 3))"], @"{2 (3 4 5 6 7)}");
    XCTAssertEqualObjects([dl rep:@"(hm1 2 (p 3))"], @"{2 3}");
    [dl rep:@"(defmacro p (x) `(let (z (atom 3)) (list z 4 5 6 7)))"];
    XCTAssertEqualObjects([dl rep:@"(hm :b @(first(p 3)))"], @"{:b 3}");
    XCTAssertEqualObjects([dl rep:@"(hm1 :a (p 5))"], @"{:a (atom 3)}");
    // test macro definition print
    XCTAssertEqualObjects([dl rep:@"(defmacro a (x) `(+ 1 ~x))"], @"user:a/1");
    XCTAssertEqualObjects([dl rep:@"(defmacro a (& more) `(first (list ~@more)))"], @"user:a/n");
    // misc
    XCTAssertEqualObjects([dl rep:@"(defun inc (x) (+ x 1))"], @"user:inc/1");
    XCTAssertEqualObjects([dl rep:@"(defmacro apply1 (x) `(apply inc/1 ~x))"], @"user:apply1/1");
    XCTAssertEqualObjects([dl rep:@"(apply1 [3])"], @"4");
    // vector binding in let
    XCTAssertEqualObjects([dl rep:@"(defmacro foo1 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([dl rep:@"(foo1)"], @"11");
    XCTAssertEqualObjects([dl rep:@"(defmacro foo2 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(nth 1 xs) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([dl rep:@"(foo2)"], @"12");
}

- (void)testMacroGensym {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmacro double (n) (let (x (gensym)) `(let (~x ~n) (+ ~x ~n))))"], @"user:double/1");
    XCTAssertEqualObjects([dl rep:@"(double 1)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(double -1)"], @"-2");
    XCTAssertEqualObjects([dl rep:@"(double 0)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(double 3.14)"], @"6.28");
}

- (void)testMacro {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defmacro unless (pred a b) `(if ~pred ~b ~a))"];
    XCTAssertEqualObjects([dl rep:@"(unless false 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(unless true 7 8)"], @"8");
    [dl rep:@"(defmacro unless2 (pred a b) `(if (not ~pred) ~a ~b))"];
    XCTAssertEqualObjects([dl rep:@"(unless2 false 7 8)"], @"7");
    XCTAssertEqualObjects([dl rep:@"(unless2 true 7 8)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(macroexpand (unless2 2 3 4))"], @"(user:if (user:not 2) 3 4)");
    XCTAssertEqualObjects([dl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(= (gensym) (gensym))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(let [or_FIXME 23] (or false (+ or_FIXME 100)))"], @"123");
    [dl rep:@"(defmacro pow2 (a) (let (x 2) `(* ~a ~x)))"];
    [dl rep:@"(defun inc2 (x) (pow2 x))"];
    XCTAssertEqualObjects([dl rep:@"(inc2 5)"], @"10");
    [dl rep:@"(defmacro p1 (a) (let (x 10) `(* ~a ~x)))"];
    [dl rep:@"(defmacro p2 (a) (let (x 2) `(p1 (* ~a ~x))))"];
    XCTAssertEqualObjects([dl rep:@"(def n (fn (x) (p2 x)))"], @"user:n/1");
    XCTAssertEqualObjects([dl rep:@"(n 4)"], @"80");
    XCTAssertEqualObjects([dl rep:@"(n 10)"], @"200");
    [dl rep:@"(defmacro p3 (a) (let (x 5) `(p2 (* ~a ~x))))"];
    [dl rep:@"(def n (fn (x) (p3 x)))"];
    XCTAssertEqualObjects([dl rep:@"(n 4)"], @"400");
    XCTAssertEqualObjects([dl rep:@"(n 5)"], @"500");
    XCTAssertEqualObjects([dl rep:@"`local-sym#"], @"user:local-sym#");
}

- (void)testVectorBasedMacros {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmacro foo1 () `(let (xs [(atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2)))]) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([dl rep:@"(defmacro foo1 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([dl rep:@"(foo1)"], @"11");
    XCTAssertEqualObjects([dl rep:@"(defmacro foo2 () `(let (xs [(atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2)))]) (@(nth 1 xs) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([dl rep:@"(defmacro foo2 () `(let (xs (vector (atom (fn (n) (+ n 1))) (atom (fn (n) (+ n 2))))) (@(nth 1 xs) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([dl rep:@"(foo2)"], @"12");
}

- (void)testErrorHandling {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLMockStdIOService *stdIOService = [DLMockStdIOService new];
    [[dl ioService] setStdIODelegate:stdIOService];
    XCTAssertThrows([dl rep:@"(throw \"err1\")"], @"Error: err1");
    XCTAssertThrows([dl rep:@"(throw {:msg \"err2\"})"], @"Error: {:msg \"err2\"}");
    XCTAssertEqualObjects([dl rep:@"(try 123 (catch e 456))"], @"123");
    @try {
        XCTAssertEqualObjects([dl rep:@"(try (abc 1 2) (catch exc (prn \"exc is:\" exc)))"], @"nil");
    } @catch (NSException *exception) {
        XCTAssertEqualObjects([dl printException:exception log:NO readably:YES], @"exc is:" "'abc' not found");
    }
    XCTAssertEqualObjects([dl rep:@"(try (throw \"my exception\") (catch exc (do (prn \"exc:\" exc) 7)))"], @"7");
    XCTAssertEqualObjects([dl rep:@"(try (abc 1 2) (catch exc (prn \"exc is:\" exc)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"exc is:\" \"'user:abc/2' not found\"");
    XCTAssertEqualObjects([dl rep:@"(try (nth 1 []) (catch exc (prn \"exc is:\" exc)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"\"exc is:\" \"Index 1 is out of bound for length 0\"");
    XCTAssertEqualObjects([dl rep:@"(try (doall (map throw/1 (list \"my err\"))) (catch exc exc))"], @"\"my err\"");
    XCTAssertEqualObjects([dl rep:@"(try (throw [\"data\" \"foo\"]) (catch exc (do (prn \"exc is:\" exc) 7)))"], @"7");
    XCTAssertEqualObjects([stdIOService output], @"\"exc is:\" [\"data\" \"foo\"]");
    XCTAssertThrows([dl rep:@"(try xyz)"], @"'xyz' not found");
    XCTAssertEqualObjects([dl rep:@"(try (throw (list 1 2 3)) (catch exc (do (prn \"err:\" exc) 7)))"], @"7");
    XCTAssertEqualObjects([stdIOService output], @"\"err:\" (1 2 3)");
}

- (void)testMeta {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLSymbol *sym = [[DLSymbol alloc] initWithName:@""];
    XCTAssertFalse([sym hasMeta]);
    id<DLDataProtocol> meta = [[DLString alloc] initWithString:@"meta-string"];
    DLSymbol *symMeta = [[DLSymbol alloc] initWithMeta:meta symbol:sym];
    XCTAssertNotNil([symMeta meta]);
    XCTAssertTrue([symMeta hasMeta]);
    XCTAssertNil([sym meta]);
    XCTAssertFalse([sym hasMeta]);
    XCTAssertEqualObjects([dl rep:@"(meta +)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta [1 2 3] {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(meta (fn (a) a))"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta (fn (a) a) {\"b\" 1}))"], @"{\"b\" 1}");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta (fn (a) a) \"abc\"))"], @"\"abc\"");
    [dl rep:@"(def l-wm (with-meta (fn (a) a) {\"b\" 2}))"];
    XCTAssertEqualObjects([dl rep:@"(meta l-wm/1)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta l-wm/1 {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([dl rep:@"(meta l-wm/1)"], @"{\"b\" 2}");
    [dl rep:@"(def f-wm (with-meta (fn [a] (+ 1 a)) {\"abc\" 1}))"];
    XCTAssertEqualObjects([dl rep:@"(meta f-wm/1)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta f-wm/1 {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([dl rep:@"(meta f-wm/1)"], @"{\"abc\" 1}");
    [dl rep:@"(def f-wm2 ^{\"abc\" 1} (fn [a] (+ 1 a)))"];
    XCTAssertEqualObjects([dl rep:@"(meta f-wm2/1)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([dl rep:@"(meta +)"], @"nil");
    // testing closures and metadata
    [dl rep:@"(def gen-plusX (fn (x) (with-meta (fn (b) (+ x b)) {\"meta\" 1})))"];
    [dl rep:@"(def plus7 (gen-plusX 7))"];
    [dl rep:@"(def plus8 (gen-plusX 8))"];
    XCTAssertEqualObjects([dl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([dl rep:@"(meta plus7/1)"], @"{\"meta\" 1}");
    XCTAssertEqualObjects([dl rep:@"(meta plus8/1)"], @"{\"meta\" 1}");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta plus7/1 {\"meta\" 2}))"], @"{\"meta\" 2}");
    XCTAssertEqualObjects([dl rep:@"(meta plus8/1)"], @"{\"meta\" 1}");
    // testing metadata on collection
    XCTAssertEqualObjects([dl rep:@"(meta [1 2 3])"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(with-meta [1 2 3] {\"a\" 1})"], @"[1 2 3]");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta [1 2 3] {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(vector? (with-meta [1 2 3] {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta [1 2 3] \"abc\"))"], @"\"abc\"");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta (list 1 2 3) {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(list? (with-meta (list 1 2 3) {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta {\"abc\" 123} {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(hash-map? (with-meta {\"abc\" 123} {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta (atom 7) {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([dl rep:@"(def l-wm (with-meta [4 5 6] {\"b\" 2}))"], @"[4 5 6]");
    XCTAssertEqualObjects([dl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([dl rep:@"(meta (with-meta l-wm {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([dl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    // testing metadata on builtin functions
    XCTAssertEqualObjects([dl rep:@"(meta +)"], @"nil");
    [dl rep:@"(def f-wm3 ^{\"fm\" 2} +)"];
    XCTAssertEqualObjects([dl rep:@"(meta f-wm3/n)"], @"{\"fm\" 2}");
    XCTAssertEqualObjects([dl rep:@"(meta +)"], @"nil");
}

/** Tests meta associated with the bound symbol */
- (void)testMetaForSymbol {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(def a (with-meta (fn (n) 1) [123]))"], @"user:a/1");
    XCTAssertEqualObjects([dl rep:@"(meta a/1)"], @"[123]");
    XCTAssertEqualObjects([dl rep:@"(meta (first (:exports (module-info \"user\"))))"], @"[123]");  // meta copied to the bound symbol
    XCTAssertEqualObjects([dl rep:@"(meta (eval (first (:exports (module-info \"user\")))))"], @"[123]");  // meta associated with the value
    XCTAssertEqualObjects([dl rep:@"(defmodule bar (export all))"], @"bar");
    XCTAssertEqualObjects([dl rep:@"(defmacro ml (n) (with-meta 1 [:a :b :c]))"], @"bar:ml/1");
    XCTAssertEqualObjects([dl rep:@"(meta ml/1)"], @"[:a :b :c]");
    XCTAssertEqualObjects([dl rep:@"(meta (first (:exports (module-info \"bar\"))))"], @"[:a :b :c]");
    XCTAssertEqualObjects([dl rep:@"(meta (eval (first (:exports (module-info \"bar\")))))"], @"[:a :b :c]");
    XCTAssertEqualObjects([dl rep:@"(defmodule foo (export all))"], @"foo");
    XCTAssertEqualObjects([dl rep:@"(defmacro m () (with-meta (fn (n) 1) [:a :b :c]))"], @"foo:m/0");
    XCTAssertEqualObjects([dl rep:@"(meta (first (:exports (module-info \"foo\"))))"], @"[:a :b :c]");
    XCTAssertEqualObjects([dl rep:@"(meta (eval (first (:exports (module-info \"foo\")))))"], @"[:a :b :c]");
}

- (void)testTCO {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(def sum2 (fn (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))"];  // tail recursive
    XCTAssertEqualObjects([dl rep:@"(sum2 10 0)"], @"55");
    XCTAssertEqualObjects([dl rep:@"(def res2 nil)"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(def res2 (sum2 10000 0))"], @"50005000");
    [dl rep:@"(def foo (fn (n) (if (= n 0) 0 (bar (- n 1)))))"];
    [dl rep:@"(def bar (fn (n) (if (= n 0) 0 (foo (- n 1)))))"];
    XCTAssertEqualObjects([dl rep:@"(foo 10000)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(do (do 1 2))"], @"2");
    [dl rep:@"(def g (fn [] 78))"];
    XCTAssertEqualObjects([dl rep:@"(g)"], @"78");
    [dl rep:@"(def g (fn [a] (+ a 78)))"];
    XCTAssertEqualObjects([dl rep:@"(g 3)"], @"81");
}

/** Returns the file from the current bundle's resources. */
- (NSString *)pathForFile:(NSString *)filename {
    NSString *path = [[NSBundle bundleForClass:[self class]] resourcePath];
    return [[NSString alloc] initWithFormat:@"%@/%@", path, filename];
}

- (void)testReadString {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLFileOps *fops = [DLFileOps new];
    XCTAssertEqualObjects([dl rep:@"(read-string \"(1 2 (3 4) nil)\")"], @"(1 2 (3 4) nil)");
    // Internally qualifies with current module name if not qualified. Eval will check core functions in core module if not in user module unless they are
    // explicitly qualified.
    XCTAssertEqualObjects([dl rep:@"(read-string \"(+ 2 3)\")"], @"(user:+ 2 3)");
    XCTAssertEqualObjects([dl rep:@"(read-string \"7 ;; comment\")"], @"7");
    XCTAssertEqualObjects([dl rep:@"(read-string \";; comment\")"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(eval (read-string \"(+ 2 3)\"))"], @"5");
    NSString *path = @"/tmp/dl-test.txt";
    [fops createFileIfNotExist:path];
    [fops openFileForAppending:path];
    [fops append:@"A line of text\n"];
    [fops closeFile];
    XCTAssertEqualObjects([dl rep:@"(slurp \"/tmp/dl-test.txt\")"], @"\"A line of text\\n\"");
    [fops delete:@"/tmp/dl-test.txt"];
    XCTAssertEqualObjects([dl rep:@"(read-string \";\")"], @"nil");
    // File read exception
    @try {
        [dl rep:@"(slurp \"foo\")"];
    } @catch (NSException *excep) {
        XCTAssertTrue([DLUtils matchString:[dl printException:excep log:YES readably:YES] withPattern:@".*No such file or directory.*"]);
    }
}

- (void)testAtom {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(def inc3 (fn (a) (+ 3 a)))"];
    XCTAssertEqualObjects([dl rep:@"(def a (atom 2))"], @"(atom 2)");
    XCTAssertEqualObjects([dl rep:@"(atom? a)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(atom? 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(deref a)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(reset! a 3)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(deref a)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(swap! a inc3/1)"], @"6");
    XCTAssertEqualObjects([dl rep:@"(deref a)"], @"6");
    XCTAssertEqualObjects([dl rep:@"(swap! a (fn (a) a))"], @"6");
    XCTAssertEqualObjects([dl rep:@"(swap! a (fn (a) (* 2 a)))"], @"12");
    XCTAssertEqualObjects([dl rep:@"(swap! a (fn (a b) (* a b)) 10)"], @"120");
    XCTAssertEqualObjects([dl rep:@"(swap! a + 3)"], @"123");
    // testing swap! closure interaction
    [dl rep:@"(def inc-it (fn (a) (+ 1 a)))"];
    [dl rep:@"(def atm (atom 7))"];
    [dl rep:@"(def f (fn () (swap! atm inc-it/1)))"];
    XCTAssertEqualObjects([dl rep:@"(f)"], @"8");
    XCTAssertEqualObjects([dl rep:@"(f)"], @"9");
    // testing `@` deref reader macro
    XCTAssertEqualObjects([dl rep:@"(def atm (atom 9))"], @"(atom 9)");
    XCTAssertEqualObjects([dl rep:@"@atm"], @"9");
    NSString *ret = [dl rep:@"(def a (atom {:x 1 :y 2}))"];
    XCTAssertTrue([ret isEqual:@"(atom {:x 1 :y 2})"] || [ret isEqual:@"(atom {:y 2 :x 1})"]);
    XCTAssertEqualObjects([dl rep:@"(get :x @a)"], @"1");
    ret = [dl rep:@"(reset! a {:x 1 :y (+ (get :y @a) 1)})"];
    XCTAssertTrue([ret isEqual:@"{:x 1 :y 3}"] || [ret isEqual:@"{:y 3 :x 1}"]);
    XCTAssertEqualObjects([dl rep:@"(def a (atom {}))"], @"(atom {})");
    XCTAssertEqualObjects([dl rep:@"(assoc @a :z 1)"], @"{:z 1}");
    [dl rep:@"(def e (atom {\"+\" +}))"];
    [dl rep:@"(swap! e assoc \"-\" -)"];
    XCTAssertEqualObjects([dl rep:@"((get \"+\" @e) 7 8)"], @"15");
    XCTAssertEqualObjects([dl rep:@"((get \"-\" @e) 11 8)"], @"3");
    [dl rep:@"(swap! e assoc \"foo\" (list))"];
    XCTAssertEqualObjects([dl rep:@"(get \"foo\" @e)"], @"()");
    [dl rep:@"(swap! e assoc \"bar\" '(1 2 3))"];
    XCTAssertEqualObjects([dl rep:@"(get \"bar\" @e)"], @"(1 2 3)");
}

- (void)testEval {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // testing eval does not use local environment
    XCTAssertEqualObjects([dl rep:@"(def a 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(let (a 2) (eval (read-string \"a\")))"], @"1");
}

- (void)testPredicate {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    DLMockStdIOService *stdIOService = [DLMockStdIOService new];
    [[dl ioService] setStdIODelegate:stdIOService];
    // nil?
    XCTAssertEqualObjects([dl rep:@"(nil? nil)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(nil? true)"], @"false");
    // true?
    XCTAssertEqualObjects([dl rep:@"(true? true)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(true? false)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(true? true?/1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(true? 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(true? \"a\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(true? :a)"], @"false");
    // false?
    XCTAssertEqualObjects([dl rep:@"(false? false)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(false? true)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(false? true)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(false? 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(false? \"a\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(false? :a)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(apply prn (list 1 2 \"3\" (list)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"1 2 \"3\" ()");
    XCTAssertEqualObjects([dl rep:@"(apply prn/n 1 2 (list \"3\" (list)))"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"1 2 (\"3\" ())");
    XCTAssertEqualObjects([dl rep:@"(apply prn/n 1 2 [\"3\" 4])"], @"nil");
    XCTAssertEqualObjects([stdIOService output], @"1 2 [\"3\" 4]");
    XCTAssertEqualObjects([dl rep:@"(apply list (list))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(apply symbol?/1 (list (quote two)))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (a b) (+ a b)) (list 2 3))"], @"5");
    XCTAssertEqualObjects([dl rep:@"(apply (fn (a b) (+ a b)) (list 4 5))"], @"9");
    XCTAssertEqualObjects([dl rep:@"(def nums (list 1 2 3))"], @"(1 2 3)");
    [dl rep:@"(def double (fn (a) (* 2 a)))"];
    XCTAssertEqualObjects([dl rep:@"(double 3)"], @"6");
    XCTAssertEqualObjects([dl rep:@"(doall (map double/1 nums))"], @"(2 4 6)");
    // symbol?
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (x) (symbol? x)) (list 1 (quote two) \"three\")))"], @"(false true false)");
    XCTAssertEqualObjects([dl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(symbol? :abc)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(symbol? (symbol \"abc\"))"], @"true");
    // keyword?
    XCTAssertEqualObjects([dl rep:@"(keyword? :abc)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(symbol \"abc\")"], @"*:abc");
    XCTAssertEqualObjects([dl rep:@"(keyword :abc)"], @":abc");
    XCTAssertEqualObjects([dl rep:@"(keyword \"abc\")"], @":abc");
    // seq?
    XCTAssertEqualObjects([dl rep:@"(seq? (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(seq? [15])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(seq? seq?/1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(seq? nil)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(seq? \"abc\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(seq? {:a 1 :b 2})"], @"false");
    XCTAssertEqualObjects([dl rep:@"(seq? 1)"], @"false");
    // hash-map?
    XCTAssertEqualObjects([dl rep:@"(hash-map? {})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(hash-map? '())"], @"false");
    XCTAssertEqualObjects([dl rep:@"(hash-map? [])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(hash-map? 'abc)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(hash-map? :abc)"], @"false");
    // string?
    XCTAssertEqualObjects([dl rep:@"(string? \"\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(string? 'abc)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(string? \"abc\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(string? :abc)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(string? (keyword \"abc\"))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(string? 234)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(string? nil)"], @"false");
    // number?
    XCTAssertEqualObjects([dl rep:@"(number? 123)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(number? -1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(number? nil)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(number? false)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(number? \"123\")"], @"false");
    // fn?
    [dl rep:@"(def add1 (fn (x) (+ x 1)))"];
    XCTAssertEqualObjects([dl rep:@"(fn? +)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(fn? add1/1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(fn? cond)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(fn? \"+\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(fn? :+)"], @"false");
    // macro?
    XCTAssertEqualObjects([dl rep:@"(macro? cond)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(macro? +)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(macro? add1/1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(macro? \"+\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(macro? :+)"], @"false");
    // zero?
    XCTAssertEqualObjects([dl rep:@"(zero? 0)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(zero? -1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(zero? (+ 1 -1))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(zero? (* 1 0))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(zero? 1)"], @"false");
    // coll?
    XCTAssertEqualObjects([dl rep:@"(coll? [])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(coll? [1 2])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(coll? '())"], @"true");
    XCTAssertEqualObjects([dl rep:@"(coll? '(1 2))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(coll? {})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(coll? {:a 1})"], @"true");
    XCTAssertEqualObjects([dl rep:@"(coll? \"\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(coll? 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(coll? nil)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(coll? (atom 0))"], @"false");
    // contains? on list
    XCTAssertEqualObjects([dl rep:@"(contains? 1 '(0 1 2))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? -1 '(0 -1 -2))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? 10 '(0 1 2))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(contains? nil '(0 1 2))"], @"false");
    XCTAssertEqualObjects([dl rep:@"(contains? nil '(0 nil 2))"], @"true");
    // contains? on vector
    XCTAssertEqualObjects([dl rep:@"(contains? 1 [0 1 2])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? -1 [0 -1 -2])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? 10 [0 1 2])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(contains? nil [0 1 2])"], @"false");
    XCTAssertEqualObjects([dl rep:@"(contains? nil [0 nil 2])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? \"a\" [0 nil 2 \"a\"])"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? \"a\" [0 nil 2 \"abc\"])"], @"false");
    // contains? on string
    XCTAssertEqualObjects([dl rep:@"(contains? \"a\" \"abc\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? \"ab\" \"abc\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? \"ab\" \"acb\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(contains? 1 \"ac1b\")"], @"true");
    // even?
    XCTAssertEqualObjects([dl rep:@"(even? 0)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(even? 1)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(even? 2)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(even? 3)"], @"false");
    XCTAssertThrows([dl rep:@"(even? 1.1)"]);
    XCTAssertThrows([dl rep:@"(even? 1.2)"]);
    XCTAssertThrows([dl rep:@"(even? 2.2)"]);
    // odd?
    XCTAssertEqualObjects([dl rep:@"(odd? 0)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(odd? 1)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(odd? 2)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(odd? 3)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(odd? 4)"], @"false");
    XCTAssertThrows([dl rep:@"(odd? 1.1)"]);
    XCTAssertThrows([dl rep:@"(odd? 1.2)"]);
    XCTAssertThrows([dl rep:@"(odd? 2.2)"]);
}

- (void)testMisc {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"nil"], @"nil");
    XCTAssertEqualObjects([dl rep:@"*host-language*"], @"\"Objective-C 2.0\"");
    [dl rep:@"(def start-time (time-ms))"];
    XCTAssertEqualObjects([dl rep:@"(= start-time 0)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(let [sumdown (fn (N) (if (> N 0) (+ N (sumdown (- N 1))) 0))] (sumdown 100)) ; Waste some time"], @"5050");  // not tail recursive
    XCTAssertEqualObjects([dl rep:@"(> (time-ms) start-time)"], @"true");
    // Object updates
    DLSymbol *s = [[DLSymbol alloc] initWithName:@"foo"];
    [s setModuleName:@"s"];
    DLSymbol *a = s;
    [a setModuleName:@"a"];
    XCTAssertNotEqualObjects([s moduleName], @"s");
    a = [s copy];
    [a setModuleName:@"b"];
    XCTAssertNotEqualObjects([s moduleName], @"s");
}

- (void)testTypeFunction {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(type \"a\")"], @"\"string\"");
    XCTAssertEqualObjects([dl rep:@"(type 1)"], @"\"number\"");
    XCTAssertEqualObjects([dl rep:@"(type 3.14)"], @"\"number\"");
    XCTAssertEqualObjects([dl rep:@"(type +)"], @"\"function\"");
    XCTAssertEqualObjects([dl rep:@"(type when)"], @"\"function\"");
    XCTAssertEqualObjects([dl rep:@"(type 'a)"], @"\"symbol\"");
    XCTAssertEqualObjects([dl rep:@"(type [1 2])"], @"\"vector\"");
    XCTAssertEqualObjects([dl rep:@"(type '(1 2))"], @"\"list\"");
    XCTAssertEqualObjects([dl rep:@"(type {:a 1})"], @"\"hash-map\"");
    XCTAssertEqualObjects([dl rep:@"(type :a)"], @"\"keyword\"");
    XCTAssertEqualObjects([dl rep:@"(type (atom 1))"], @"\"atom\"");
    XCTAssertEqualObjects([dl rep:@"(type (fn () 1))"], @"\"function\"");
    XCTAssertEqualObjects([dl rep:@"(def a 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(type a)"], @"\"number\"");
    XCTAssertEqualObjects([dl rep:@"(type nil)"], @"\"nil\"");
}

- (void)testErrorMessages {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // data type error
    XCTAssertEqualObjects([dl rep:@"(try (+ \"\") (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'string'\"");
    XCTAssertEqualObjects([dl rep:@"(try (+ []) (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'vector'\"");
    XCTAssertEqualObjects([dl rep:@"(try (+ [] \"\") (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'vector'\"");
    XCTAssertEqualObjects([dl rep:@"(try (+ '()) (catch ex (str ex)))"], @"\"Expected 'number' but obtained 'list'\"");
    XCTAssertEqualObjects([dl rep:@"(try (empty? 1) (catch ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'number'\"");
    XCTAssertEqualObjects([dl rep:@"(try (empty? 1.0) (catch ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'number'\"");
    XCTAssertEqualObjects([dl rep:@"(try (empty? (atom 1)) (catch ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'atom'\"");
    XCTAssertEqualObjects([dl rep:@"(try (first true) (catch ex (str ex)))"], @"\"'first/1' requires 'sequence' for argument 1 but obtained 'bool'\"");
    // Function not found
    XCTAssertEqualObjects([dl rep:@"(try (abc [1 2 3]) (catch ex (str ex)))"], @"\"'user:abc/1' not found\"");
    // Arity error
    XCTAssertEqualObjects([dl rep:@"(try (empty? [] []) (catch ex (str ex)))"], @"\"'user:empty?/2' not found\"");
    XCTAssertEqualObjects([dl rep:@"(try (list? [] []) (catch ex (str ex)))"], @"\"'user:list?/2' not found\"");
    XCTAssertEqualObjects([dl rep:@"(try (true? [] []) (catch ex (str ex)))"], @"\"'user:true?/2' not found\"");
    XCTAssertEqualObjects([dl rep:@"(def a (atom 4))"], @"(atom 4)");
    XCTAssertEqualObjects([dl rep:@"(try (reset! x 4) (catch ex (str ex)))"], @"\"'user:x' not found\"");
}

/** The expressions loaded and evaluated from file should be added to the env just like it is evaluated from REPL. */
- (void)testScopeWithFile {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    NSString *scopePath = [self pathForFile:@"scope.dlisp"];
    XCTAssertTrue([scopePath isNotEmpty]);
    NSString *ret = [dl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", scopePath]];
    XCTAssertEqualObjects(ret, @"[:ok \"scope.dlisp\"]");
    XCTAssertEqualObjects([dl rep:@"(f1 4 6)"], @"30");
}

- (void)testScope {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(def a 4)"], @"4");
    XCTAssertEqualObjects([dl rep:@"(def c 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(def d (atom 3))"], @"(atom 3)");
    [dl rep:@"(def f1 (fn (a b) (do (+ a b c (deref d)) (let (x 6 z (+ a c) f (fn (x) (* x x))) (* x z)))))"];
    XCTAssertEqualObjects([dl rep:@"(f1 4 6)"], @"30");
}

- (void)testModule {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))"];
    [dl rep:@"(def a 1)"];
    XCTAssertEqualObjects([dl rep:@"a"], @"1");
    [dl rep:@"(in-module \"user\")"];
}

- (void)testMacroWithModules {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defmacro d (n v) `(def ~n ~v))"];
    XCTAssertEqualObjects([dl rep:@"(d x 4)"], @"4");
    XCTAssertEqualObjects([dl rep:@"(defmodule foo ())"], @"foo");
    XCTAssertEqualObjects([dl rep:@"(user:d t 2)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    [dl rep:@"(in-module \"user\")"];
    XCTAssertThrows([dl rep:@"(foo:random-1 4)"]);  // function not exported
}

- (void)testModuleExports {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmodule foo (export (inc 1)) (export (dec 1)))"], @"foo");
    XCTAssertEqualObjects([dl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([dl rep:@"(inc 4)"], @"5");
    XCTAssertEqualObjects([dl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([dl rep:@"(defun greet () 42)"], @"foo:greet/0");
    XCTAssertEqualObjects([dl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(foo:inc 4)"], @"5");
    XCTAssertThrows([dl rep:@"(foo:random-2)"]);  // function not exported
}

- (void)testModuleExportAll {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmodule foo (export (inc 1)) (export all))"], @"foo");
    XCTAssertEqualObjects([dl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([dl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([dl rep:@"(defun greet () 42)"], @"foo:greet/0");
    XCTAssertEqualObjects([dl rep:@"(inc 4)"], @"5");
    XCTAssertEqualObjects([dl rep:@"(dec 4)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(foo:inc 41)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(foo:dec 43)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(foo:greet)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(defmodule bar (export (sum 2)))"], @"bar");
    XCTAssertEqualObjects([dl rep:@"(defun sum (x y) (+ x y))"], @"bar:sum/2");
    XCTAssertEqualObjects([dl rep:@"(defun diff (x y) (- x y))"], @"bar:diff/2");
    XCTAssertEqualObjects([dl rep:@"(sum 40 2)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(diff 45 3)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(bar:sum 40 2)"], @"42");
    XCTAssertThrows([dl rep:@"(bar:diff 45 3)"]);  // function not exported
    XCTAssertEqualObjects([dl rep:@"(in-module \"foo\")"], @"\"foo\"");
    XCTAssertEqualObjects([dl rep:@"(bar:sum 40 2)"], @"42");
    XCTAssertThrows([dl rep:@"(bar:diff 45 3)"]);  // function not exported
}

- (void)testModuleImport {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmodule bar (export (sum 2) (sum 1)))"], @"bar");
    XCTAssertEqualObjects([dl rep:@"(defun sum (x y) (+ x y))"], @"bar:sum/2");
    XCTAssertEqualObjects([dl rep:@"(defun sum (x) (+ x 10))"], @"bar:sum/1");
    XCTAssertEqualObjects([dl rep:@"(defmodule foo (export (inc 1) (greet 0) (a 0)) (import (from bar (sum 2) (sum 1))))"], @"foo");
    XCTAssertEqualObjects([dl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([dl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([dl rep:@"(defun greet () (sum 32))"], @"foo:greet/0");
    XCTAssertEqualObjects([dl rep:@"(defmacro a () 1)"], @"foo:a/0");
    XCTAssertEqualObjects([dl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([dl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(foo:greet)"], @"42");
    // Invoke MFA from string format
    XCTAssertEqualObjects([dl rep:@"(eval (read-string \"(foo:greet)\"))"], @"42");
}

- (void)testRemoveModule {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmodule rfoo (export (sum 2)))"], @"rfoo");
    XCTAssertEqualObjects([dl rep:@"(defun sum (x y) (+ x y))"], @"rfoo:sum/2");
    XCTAssertEqualObjects([dl rep:@"(in-module \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(rfoo:sum 10 20)"], @"30");
    XCTAssertEqualObjects([dl rep:@"(remove-module \"rfoo\")"], @"nil");
    XCTAssertThrows([dl rep:@"(rfoo:sum 45 3)"]);
    XCTAssertThrows([dl rep:@"(in-module \"rfoo\")"]);
}

- (void)testExportSymbolResolveFault {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(defmodule foo (export all))"];
    XCTAssertEqualObjects([DLState.shared currentModuleName], @"foo");
    DLEnv *fooEnv = [DLEnv envForModuleName:@"foo"];
    XCTAssertNotNil(fooEnv);
    XCTAssertEqualObjects([dl rep:@"(defun fa (n) n)"], @"foo:fa/1");
    XCTAssertEqualObjects([dl rep:@"(fa 21)"], @"21");
    [dl rep:@"(in-module \"user\")"];
    XCTAssertEqualObjects([dl rep:@"(foo:fa 21)"], @"21");
    [dl rep:@"(defmodule bar (export (ba 1) (bb 1)))"];
    XCTAssertEqualObjects([DLState.shared currentModuleName], @"bar");
    DLEnv *barEnv = [DLEnv envForModuleName:@"bar"];
    XCTAssertNotNil(barEnv);
    NSArray *barKeys = [[barEnv exportTable] allKeys];
    XCTAssertEqual([barKeys count], 2);
    DLSymbol *sym = [barKeys firstObject];
    id<DLDataProtocol> elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertTrue([DLFault isFault:elem]);
    XCTAssertEqualObjects([dl rep:@"(defun ba (n) (+ n 1))"], @"bar:ba/1");
    XCTAssertEqualObjects([dl rep:@"(defun bb (n) (- n 1))"], @"bar:bb/1");
    XCTAssertEqualObjects([dl rep:@"(ba 21)"], @"22");
    XCTAssertEqualObjects([dl rep:@"(bb 21)"], @"20");
    [dl rep:@"(in-module \"foo\")"];
    [fooEnv resolveFault:elem forKey:sym inEnv:barEnv];
    elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertFalse([DLFault isFault:elem]);
    XCTAssertEqualObjects([dl rep:@"(bar:ba 21)"], @"22");
    sym = [[barKeys filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.value contains [c] %@", @"bb"]] firstObject];
    elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertTrue([DLFault isFault:elem]);
    XCTAssertEqualObjects([dl rep:@"(bar:bb 21)"], @"20");
    sym = [[[[barEnv exportTable] allKeys] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF.value contains [c] %@", @"bb"]] firstObject];
    elem = [[barEnv exportTable] objectForSymbol:sym];
    XCTAssertNotNil(elem);
    XCTAssertFalse([DLFault isFault:elem]);
    [dl rep:@"(in-module \"user\")"];
    XCTAssertEqualObjects([dl rep:@"(bar:ba 21)"], @"22");
    XCTAssertEqualObjects([dl rep:@"(bar:bb 21)"], @"20");
}

- (void)testCodeLoadedFromFile {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    NSString *moduleTest = [self pathForFile:@"module-test.dlisp"];
    XCTAssertTrue([moduleTest isNotEmpty]);
    NSString *ret = [dl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", moduleTest]];
    XCTAssertEqualObjects(ret, @"[:ok \"module-test.dlisp\"]");
    XCTAssertEqualObjects([dl rep:@"(bbar:bba 5)"], @"6");
    XCTAssertEqualObjects([dl rep:@"(bbaz:zza 5)"], @"25");
    XCTAssertEqualObjects([dl rep:@"(bbaz:zzb 5)"], @"30");
    XCTAssertEqualObjects([dl rep:@"(bbaz:zzc 5)"], @"30");
}

- (void)testModuleFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // Test module-info
    XCTAssertEqualObjects([dl rep:@"(defmodule ifoo (export (ifa 1) (ifa 2)) (import (from core (empty? 1))))"], @"ifoo");
    [dl rep:@"(defun iinc (n) (+ n 1))"];
    [dl rep:@"(def info (module-info \"ifoo\"))"];
    NSString *count = [dl rep:@"(count (get :exports info))"];
    XCTAssertEqual([count integerValue], 2);
    count = [dl rep:@"(count (get :imports info))"];
    XCTAssertEqual([count integerValue], 1);
    count = [dl rep:@"(count (get :internal info))"];
    XCTAssertEqual([count integerValue], 1);
    XCTAssertEqualObjects([dl rep:@"(current-module-name)"], @"\"ifoo\"");
    XCTAssertThrows([dl rep:@"(in-module \"nope.ifoo\")"]);
    // Test module-info
    XCTAssertEqualObjects([dl rep:@"(:name (module-info (current-module-name)))"], @"\"ifoo\"");
    // Test module-exist?
    XCTAssertEqualObjects([dl rep:@"(module-exist? \"nope.ifoo\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(module-exist? \"core\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(module-exist? \"user\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(module-exist? \"ifoo\")"], @"true");
    XCTAssertEqualObjects([dl rep:@"(remove-module \"ifoo\")"], @"nil");
    XCTAssertEqualObjects([dl rep:@"(module-exist? \"ifoo\")"], @"false");
    XCTAssertEqualObjects([dl rep:@"(current-module-name)"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(in-module \"core\")"], @"\"core\"");
    XCTAssertEqualObjects([dl rep:@"(current-module-name)"], @"\"core\"");
    XCTAssertEqualObjects([dl rep:@"(def mod-name \"user\")"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(in-module mod-name)"], @"\"user\"");
    // Test all-modules
    [dl rep:@"(def modules (all-modules))"];
    XCTAssertGreaterThanOrEqual([[dl rep:@"(count modules)"] integerValue], 2);
    XCTAssertEqualObjects([dl rep:@"(contains? \"core\" modules)"], @"true");
}

- (void)testModuleDescription {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmodule mdesc \"A test module.\" (export all))"], @"mdesc");
    DLEnv *mdescEnv = [DLEnv envForModuleName:@"mdesc"];
    XCTAssertEqualObjects([mdescEnv moduleDescription], @"A test module.");
    [dl rep:@"(def info (module-info \"mdesc\"))"];
    XCTAssertEqualObjects([dl rep:@"(get :description info)"], @"\"A test module.\"");
}

- (void)testModuleArity {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(defmodule foo.arity (export (foo 1) (bar n)))"], @"foo.arity");
    XCTAssertEqualObjects([dl rep:@"(defun bar (& more) more)"], @"foo.arity:bar/n");
    XCTAssertEqualObjects([dl rep:@"(defun foo (n) n)"], @"foo.arity:foo/1");
    XCTAssertEqualObjects([dl rep:@"(defmodule bar.arity (export all) (import (from foo.arity (bar n))))"], @"bar.arity");
    XCTAssertEqualObjects([dl rep:@"(defun zoo (n) (bar 1 2 n))"], @"bar.arity:zoo/1");
    XCTAssertEqualObjects([dl rep:@"(foo.arity:foo 1)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(foo.arity:bar 1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(bar.arity:zoo 3)"], @"(1 2 3)");
}

- (void)testInfo {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(def a 3)"], @"3");
    [dl rep:@"(def a-info (info a))"];
    XCTAssertEqualObjects([dl rep:@"(:type a-info)"], @"\"number\"");
    XCTAssertEqualObjects([dl rep:@"(:module a-info)"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(:position a-info)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(def s (symbol \"foo:bar/n\"))"], @"user:bar/n");
    [dl rep:@"(def s-info (info s))"];
    XCTAssertEqualObjects([dl rep:@"(:type s-info)"], @"\"symbol\"");
    XCTAssertEqualObjects([dl rep:@"(:module s-info)"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(:initial-module s-info)"], @"\"foo\"");
    XCTAssertEqualObjects([dl rep:@"(:qualified? s-info)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(:function? s-info)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(:arity s-info)"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(def f (fn (n) (+ n 3)))"], @"user:f/1");
    [dl rep:@"(def f-info (info user:f/1))"];
    XCTAssertEqualObjects([dl rep:@"(:type f-info)"], @"\"function\"");
    XCTAssertEqualObjects([dl rep:@"(:module f-info)"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(:arity f-info)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(:macro? f-info)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(defmacro m (n) (+ n 3))"], @"user:m/1");
    [dl rep:@"(def m-info (info user:m/1))"];
    XCTAssertEqualObjects([dl rep:@"(:type m-info)"], @"\"function\"");
    XCTAssertEqualObjects([dl rep:@"(:module m-info)"], @"\"user\"");
    XCTAssertEqualObjects([dl rep:@"(:arity m-info)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(:macro? m-info)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(def z (atom {:a 4}))"], @"(atom {:a 4})");
    [dl rep:@"(def z-info (info z))"];
    XCTAssertEqualObjects([dl rep:@"(:type z-info)"], @"\"atom\"");
    XCTAssertEqualObjects([dl rep:@"(:value-type z-info)"], @"\"hash-map\"");
}

- (void)testSortInternal {
    // dlist
    DLNumber *n1 = [[DLNumber alloc] initWithInteger:-1];
    DLNumber *n2 = [[DLNumber alloc] initWithInteger:7];
    DLNumber *n3 = [[DLNumber alloc] initWithInteger:3];
    DLNumber *n4 = [[DLNumber alloc] initWithInteger:2];
    DLList *list = [[DLList alloc] initWithArray:[@[n1, n2, n3, n4] mutableCopy]];
    DLList *sorted = [list sort:dl_sortAscending];
    XCTAssertEqualObjects([sorted first], n1);
    sorted = [list sort:dl_sortDescending];
    XCTAssertEqualObjects([sorted first], n2);
}

- (void)testSort {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(sort :asc '(3 5 2 -1 8))"], @"(-1 2 3 5 8)");
    XCTAssertEqualObjects([dl rep:@"(sort :desc '(3 5 2 -1 8))"], @"(8 5 3 2 -1)");
    XCTAssertEqualObjects([dl rep:@"(sort :asc [3 5 2 -1 8])"], @"[-1 2 3 5 8]");
    XCTAssertEqualObjects([dl rep:@"(sort :desc [3 5 2 -1 8])"], @"[8 5 3 2 -1]");
    XCTAssertEqualObjects([dl rep:@"(sort :asc \"We are Legends\")"], @"\"  LWadeeeegnrs\"");
    XCTAssertEqualObjects([dl rep:@"(sort :desc [\"We\" \"are\" \"Legends\"])"], @"[\"Legends\" \"are\" \"We\"]");
    XCTAssertEqualObjects([dl rep:@"(sort [:value (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1))] {:x \"We\" :y \"are\" :z \"Legends\"})"],
                          @"[\"Legends\" \"We\" \"are\"]");
    XCTAssertEqualObjects([dl rep:@"(sort (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1)) [\"We\" \"are\" \"Legends\"])"], @"[\"Legends\" \"We\" \"are\"]");
    XCTAssertEqualObjects([dl rep:@"(sort [:key :asc] {:z 2 :a 4 :p -5})"], @"[:a :p :z]");
    XCTAssertEqualObjects([dl rep:@"(sort [:key :desc] {:z 2 :a 4 :p -5})"], @"[:z :p :a]");
    XCTAssertEqualObjects([dl rep:@"(sort [:value :asc] {:z 2 :a 4 :p -5})"], @"[-5 2 4]");
    XCTAssertEqualObjects([dl rep:@"(sort [:value :desc] {:z 2 :a 4 :p -5})"], @"[4 2 -5]");
    XCTAssertEqualObjects([dl rep:@"(sort :asc [3 5 2 (atom -1) -1 8])"], @"[(atom -1) -1 2 3 5 8]");
    // Implicit sort
    XCTAssertEqualObjects([dl rep:@"(sort {:a 3 :s 4 :g 5})"], @"[:a :g :s]");
    XCTAssertEqualObjects([dl rep:@"(sort '(3 5 2 -1 8))"], @"(-1 2 3 5 8)");
    XCTAssertEqualObjects([dl rep:@"(sort [1 6 2 7 4])"], @"[1 2 4 6 7]");
}

- (void)testFilter {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(def hm1 (doall (filter (fn (m) (zero? (mod (first (values m)) 2))) {:a 1 :b 2 :c 3 :d 4})))"];
    [dl rep:@"(def hm2 (doall (filter (fn (m) (zero? (mod (first (keys m)) 2))) {1 10 2 20 3 30 4 40})))"];
    XCTAssertEqualObjects([dl rep:@"(sort [:key :asc] hm1)"], @"[:b :d]");
    XCTAssertEqualObjects([dl rep:@"(sort [:value :asc] hm1)"], @"[2 4]");
    XCTAssertEqualObjects([dl rep:@"(sort [:key :asc] hm2)"], @"[2 4]");
    XCTAssertEqualObjects([dl rep:@"(sort [:value :asc] hm2)"], @"[20 40]");
    XCTAssertEqualObjects([dl rep:@"(doall (filter (fn (x) (> x 0)) [-1 4 0 -4 -5 2]))"], @"[4 2]");
    XCTAssertEqualObjects([dl rep:@"(doall (filter (fn (x) (<= x 0)) '(-1 4 0 -4 -5 2)))"], @"(-1 0 -4 -5)");
    [dl rep:@"(def str-xs [\"It\" \"is\" \"an\" \"ancient\" \"Mariner\" \"And\" \"he\" \"stoppeth\" \"one\" \"of\" \"three\"])"];
    XCTAssertEqualObjects([dl rep:@"(doall (filter (fn (x) (= (count x) 3)) str-xs))"], @"[\"And\" \"one\"]");
}

- (void)testPartition {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(def hmv1 (partition (fn (k v) (= (mod v 2) 0)) {:a 1 :b 2 :c 3 :d 4}))"];
    [dl rep:@"(def hmv2 (partition (fn (k v) (= (mod k 2) 0)) {1 10 2 20 3 30 4 40}))"];
    XCTAssertEqualObjects([dl rep:@"(count (first hmv1))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(count (second hmv1))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(sort [:key :asc] (first hmv1))"], @"[:b :d]");
    XCTAssertEqualObjects([dl rep:@"(sort [:key :asc] (second hmv1))"], @"[:a :c]");
    XCTAssertEqualObjects([dl rep:@"(partition (fn (x) (> x 4)) [4 2 5 3 1])"], @"[[5] [4 2 3 1]]");
}

- (void)testFlatten {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(doall (flatten [[1] [2 [3]] [[[4]]] [5]]))"], @"[1 2 3 4 5]");
    XCTAssertEqualObjects([dl rep:@"(doall (flatten '('(1) '(2 '(3)) '('('(4))) '(5))))"],
                          @"(user:quote/1 1 user:quote/1 2 user:quote/1 3 user:quote/1 user:quote/1 user:quote/1 4 user:quote/1 5)");
    XCTAssertEqualObjects([dl rep:@"(doall (flatten []))"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(doall (flatten '()))"], @"()");
    XCTAssertEqualObjects([dl rep:@"(doall (flatten '((+ 1 2) 3)))"], @"(user:+ 1 2 3)");
    [dl rep:@"(def hm (flatten {:a 1 :b {:c 2 :d 3} :e 4}))"];
    XCTAssertEqualObjects([dl rep:@"(contains? :a hm)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? :b hm)"], @"false");
    XCTAssertEqualObjects([dl rep:@"(contains? :c hm)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? :d hm)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(contains? :e hm)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(count hm)"], @"4");
    XCTAssertThrows([dl rep:@"(flatten nil)"]);
}

- (void)testJoin {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(join \"->\" \"abcd\")"], @"\"a->b->c->d\"");
    XCTAssertEqualObjects([dl rep:@"(join \"->\" [1 2 3])"], @"[1 \"->\" 2 \"->\" 3]");
    XCTAssertEqualObjects([dl rep:@"(join 0 '(1 2 3))"], @"(1 0 2 0 3)");
    XCTAssertEqualObjects([dl rep:@"(join nil '(1 2 3))"], @"(1 nil 2 nil 3)");
    XCTAssertEqualObjects([dl rep:@"(join {:b 4} [1 2 3])"], @"[1 [[:b 4]] 2 [[:b 4]] 3]");
    XCTAssertEqualObjects([dl rep:@"(sort (join {:b 4} {:a 2 :x 5}))"], @"[[[:b 4]] [:a 2] [:x 5]]");
    XCTAssertThrows([dl rep:@"(join 0 1)"]);
}

- (void)testInto {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // list
    XCTAssertEqualObjects([dl rep:@"(into '() nil)"], @"()");
    XCTAssertEqualObjects([dl rep:@"(into '() '(1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([dl rep:@"(into '(1 2) '(3 4))"], @"(1 2 3 4)");
    XCTAssertEqualObjects([dl rep:@"(into '(1 2) \"a\")"], @"(1 2 \"a\")");
    XCTAssertEqualObjects([dl rep:@"(into '(1 2) \"abc\")"], @"(1 2 \"abc\")");
    XCTAssertEqualObjects([dl rep:@"(into '() {:a 1})"], @"((:a 1))");
    XCTAssertEqualObjects([dl rep:@"(into '(1 2) {:a 1})"], @"(1 2 (:a 1))");
    XCTAssertThrows([dl rep:@"(into '(1 2) 1)"]);
    // vector
    XCTAssertEqualObjects([dl rep:@"(into [] nil)"], @"[]");
    XCTAssertEqualObjects([dl rep:@"(into [] '(1 2 3))"], @"[1 2 3]");
    XCTAssertEqualObjects([dl rep:@"(into [1 2] '(3 4))"], @"[1 2 3 4]");
    XCTAssertEqualObjects([dl rep:@"(into [1 2] \"a\")"], @"[1 2 \"a\"]");
    XCTAssertEqualObjects([dl rep:@"(into [1 2] \"abc\")"], @"[1 2 \"abc\"]");
    XCTAssertEqualObjects([dl rep:@"(into [] {:a 1})"], @"[[:a 1]]");
    XCTAssertEqualObjects([dl rep:@"(into [1 2] {:a 1})"], @"[1 2 [:a 1]]");
    XCTAssertThrows([dl rep:@"(into [1 2] 1)"]);
    // hash-map
    XCTAssertEqualObjects([dl rep:@"(into {} nil)"], @"{}");
    XCTAssertEqualObjects([dl rep:@"(into {} [:a 1])"], @"{:a 1}");
    XCTAssertEqualObjects([dl rep:@"(count (into {:a 1} [:b 2]))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(into {} '(1 2))"], @"{1 2}");
    XCTAssertEqualObjects([dl rep:@"(into {} {:a 1})"], @"{:a 1}");
    XCTAssertEqualObjects([dl rep:@"(count (into {:a 1} {:b 2}))"], @"2");
    XCTAssertThrows([dl rep:@"(into {} 1)"]);
}

- (void)testIndexOf {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    //list
    XCTAssertEqualObjects([dl rep:@"(index-of 1 '(0 1 2))"], @"1");
    XCTAssertEqualObjects([dl rep:@"(index-of -1 '(0 1 -1 4 -5))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(index-of 1 '())"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(index-of nil '())"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(index-of nil '(nil))"], @"0");
    // vector
    XCTAssertEqualObjects([dl rep:@"(index-of 1 [0 1 2])"], @"1");
    XCTAssertEqualObjects([dl rep:@"(index-of \"abc\" '(\"ab\" \"z\" \"abc\"))"], @"2");
    XCTAssertEqualObjects([dl rep:@"(index-of {:a 1} [{:b 2} {:c 3} {:a 1}])"], @"2");
    // string
    XCTAssertEqualObjects([dl rep:@"(index-of \"a\" \"abc\")"], @"0");
    XCTAssertEqualObjects([dl rep:@"(index-of \"abc\" \"fooabc\")"], @"3");
    XCTAssertEqualObjects([dl rep:@"(index-of \"a\" \"zbc\")"], @"-1");
    XCTAssertEqualObjects([dl rep:@"(index-of \"a\" \"bc ef ab ba\")"], @"6");
    XCTAssertThrows([dl rep:@"(index-of 1 {})"]);
    XCTAssertThrows([dl rep:@"(index-of 1 1)"]);
    XCTAssertThrows([dl rep:@"(index-of 1 :a)"]);
}

- (void)testMap {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // single arity function
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a) (+ a 2)) '(1 2 3 4)))"], @"(3 4 5 6)");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a) (+ a 2)) [1 2 3 4]))"], @"[3 4 5 6]");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a) (str a \"ok\")) \"abcd\"))"], @"\"aokbokcokdok\"");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a) a) {:a 1}))"], @"{:a 1}");
    // multi arity function
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) [a b]) [1 2] '(3 4)))"], @"[[1 3] [2 4]]");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) (list a b)) [1 2] '(3 4)))"], @"[(1 3) (2 4)]");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) (list a b)) '(1 2) [3 4]))"], @"((1 3) (2 4))");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) (+ a b)) [1 2] '(3 4)))"], @"[4 6]");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) (+ a b)) '(1 2) [3 4]))"], @"(4 6)");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) (str a b)) \"abc\" \"xyz\"))"], @"\"axbycz\"");
    XCTAssertEqualObjects([dl rep:@"(doall (map (fn (a b) [a b]) {:a 1} {:x 3}))"], @"[{:a 1} {:x 3}]");
    XCTAssertEqualObjects([dl rep:@"(count (doall (flatten (doall (map (fn (a b) [a b]) {:a 1 :b 2} {:c 3 :d 4})))))"], @"4");
}

- (void)testFold {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // foldl
    XCTAssertEqualObjects([dl rep:@"(foldl (fn (x acc) (+ x acc)) 0 [1 2 3])"], @"6");
    XCTAssertEqualObjects([dl rep:@"(foldl (fn (x acc) (+ x acc)) 10 [])"], @"10");
    XCTAssertEqualObjects([dl rep:@"(foldl (fn (x acc) [(+ x (first acc))]) [0] [1 2 3])"], @"[6]");
    XCTAssertEqualObjects([dl rep:@"(foldl (fn (x acc) (str acc x)) \"\" [\"a\" 1 \"b\" 2 \"c\" 3])"], @"\"a1b2c3\"");
    // foldr
    XCTAssertEqualObjects([dl rep:@"(foldr (fn (x acc) (+ x acc)) 0 [1 2 3])"], @"6");
    XCTAssertEqualObjects([dl rep:@"(foldr (fn (x acc) (+ x acc)) 10 [])"], @"10");
    XCTAssertEqualObjects([dl rep:@"(foldr (fn (x acc) [(+ x (first acc))]) [0] [1 2 3])"], @"[6]");
    XCTAssertEqualObjects([dl rep:@"(foldr (fn (x acc) (str acc x)) \"\" [\"a\" 1 \"b\" 2 \"c\" 3])"], @"\"3c2b1a\"");
    // hash-map
    [dl rep:@"(def hm {:a 1 :b 2 :c 3})"];
    [dl rep:@"(def hm-ret (foldr (fn (x y acc) (println x y acc) (assoc acc x y)) {} hm))"];
    XCTAssertEqualObjects([dl rep:@"(= hm hm-ret)"], @"true");
}

- (void)testThreadingMacro {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // thread first ->
    XCTAssertEqualObjects([dl rep:@"(-> [1 2 3] (count))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(-> \"abc\" (concat \"xyz\"))"], @"\"abcxyz\"");
    XCTAssertEqualObjects([dl rep:@"(-> [1 4] (concat [2 3]))"], @"[1 4 2 3]");
    // thread last <-
    XCTAssertEqualObjects([dl rep:@"(<- \"abc\" (concat \"xyz\"))"], @"\"xyzabc\"");
    XCTAssertEqualObjects([dl rep:@"(<- [1 4] (concat [2 3]))"], @"[2 3 1 4]");
}

- (void)testCoreLibFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // inc
    XCTAssertEqualObjects([dl rep:@"(inc 1)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(inc 1.1)"], @"2.1");
    XCTAssertEqualObjects([dl rep:@"(inc -1.0)"], @"0.0");
    // dec
    XCTAssertEqualObjects([dl rep:@"(dec 1)"], @"0");
    XCTAssertEqualObjects([dl rep:@"(dec 1.1)"], @"0.1");
    XCTAssertEqualObjects([dl rep:@"(dec -1.0)"], @"-2.0");
    // identity
    XCTAssertEqualObjects([dl rep:@"(identity -1.0)"], @"-1.0");
    XCTAssertEqualObjects([dl rep:@"(identity [1 2 3])"], @"[1 2 3]");
}

- (void)testLazySequenceCoreFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // lazy-seq
    [dl rep:@"(def lseq (lazy-seq [1 2 3]))"];
    XCTAssertEqualObjects([dl rep:@"(type lseq)"], @"\"lazy-sequence\"");
    XCTAssertEqualObjects([dl rep:@"(type (lazy-seq '(1 2 3)))"], @"\"lazy-sequence\"");
    XCTAssertEqualObjects([dl rep:@"(type (lazy-seq \"abc\"))"], @"\"lazy-sequence\"");
    XCTAssertThrows([dl rep:@"(lazy-seq 1)"]);
    // lazy-seq?
    XCTAssertEqualObjects([dl rep:@"(lazy-seq? lseq)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(lazy-seq? [1 2 3])"], @"false");
    // has-next?, next
    XCTAssertEqualObjects([dl rep:@"(has-next? lseq)"], @"true");
    XCTAssertEqualObjects([dl rep:@"(next lseq)"], @"1");
    XCTAssertEqualObjects([dl rep:@"(next lseq)"], @"2");
    XCTAssertEqualObjects([dl rep:@"(next lseq)"], @"3");
    XCTAssertEqualObjects([dl rep:@"(has-next? lseq)"], @"false");
    XCTAssertThrows([dl rep:@"(next lseq)"]);
    // flatten, doall
    XCTAssertEqualObjects([dl rep:@"(type (flatten [1 [2 4] 3]))"], @"\"lazy-sequence\"");
    XCTAssertEqualObjects([dl rep:@"(doall (flatten [1 [2 4] 3]))"], @"[1 2 4 3]");
    XCTAssertEqualObjects([dl rep:@"(doall [1 [2 4] 3])"], @"[1 [2 4] 3]");
}

- (void)testTestLazyFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // take on a lazy sequence
    XCTAssertEqualObjects([dl rep:@"(take 2 (map (fn (a) a) [1 2 3 4]))"], @"[1 2]");
    XCTAssertEqualObjects([dl rep:@"(take 2 (map (fn (a) a) \"abcd\"))"], @"\"ab\"");
    // first
    XCTAssertEqualObjects([dl rep:@"(first (map (fn (a) a) [1 2 3 4]))"], @"1");
    XCTAssertEqualObjects([dl rep:@"(first (map (fn (a) a) \"abc\"))"], @"\"a\"");
    // rest
    XCTAssertEqualObjects([dl rep:@"(rest (map (fn (a) a) [1 2 3 4]))"], @"[2 3 4]");
    XCTAssertEqualObjects([dl rep:@"(rest (map (fn (a) a) '(1 2 3 4)))"], @"(2 3 4)");
    XCTAssertEqualObjects([dl rep:@"(rest (map (fn (a) a) \"abc\"))"], @"\"bc\"");
    // nth
    XCTAssertEqualObjects([dl rep:@"(nth 2 (map (fn (a) a) [1 2 3 4]))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(nth 2 (map (fn (a) a) \"abc\"))"], @"\"c\"");
    // nth-tail
    XCTAssertEqualObjects([dl rep:@"(nth-tail 1 3 (map (fn (a) a) [1 2 3 4 5]))"], @"[2 3 4]");
    XCTAssertEqualObjects([dl rep:@"(nth-tail 1 3 (map (fn (a) a) \"abcdef\"))"], @"\"bcd\"");
    // last
    XCTAssertEqualObjects([dl rep:@"(last (map (fn (a) a) [1 2 3]))"], @"3");
    XCTAssertEqualObjects([dl rep:@"(last (map (fn (a) a) \"abc\"))"], @"\"c\"");
    // drop
    XCTAssertEqualObjects([dl rep:@"(lazy-seq? (drop 2 (map (fn (a) a) [1 2 3 4])))"], @"true");
    XCTAssertEqualObjects([dl rep:@"(doall (drop 2 (map (fn (a) a) [1 2 3 4])))"], @"[3 4]");
    XCTAssertEqualObjects([dl rep:@"(doall (drop 2 (map (fn (a) a) \"abcd\")))"], @"\"cd\"");
}

- (void)testHashMapToFoundation {
    DLHashMap *hm = [DLHashMap new];
    [hm setObject:[DLString stringWithString:@"111"] forKey:[DLKeyword keywordWithString:@"a1"]];
    [hm setObject:[DLString stringWithString:@"222"] forKey:[DLKeyword keywordWithString:@"b2"]];
    [hm setObject:[DLString stringWithString:@"333"] forKey:[DLKeyword keywordWithString:@"c3"]];
    DLHashMap *hm1 = [DLHashMap new];
    [hm1 setObject:[[DLList alloc] initWithArray:[@[@(1), @(2), @(3)] mutableCopy]] forKey:[DLString stringWithString:@"d4"]];
    [hm1 setObject:[[DLNumber alloc] initWithDouble:3.1415] forKey:[DLString stringWithString:@"e5"]];
    [hm1 setObject:[[DLBool alloc] initWithBool:YES] forKey:[[DLNumber alloc] initWithInt:6]];
    [hm setObject:hm1 forKey:[DLKeyword keywordWithString:@"f7"]];
    NSMutableDictionary *dict = [DLUtils hashMapToFoundationType:hm];
    NSError *err = nil;
    NSData *json = [NSJSONSerialization dataWithJSONObject:dict options:NSJSONWritingSortedKeys error:&err];
    XCTAssertNil(err);
    NSString *jsonString = [[NSString alloc] initWithData:json encoding:NSUTF8StringEncoding];
    XCTAssertNotNil(jsonString);
    XCTAssertGreaterThanOrEqual([jsonString count], 70);
}

- (void)testJSONSerialization {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    [dl rep:@"(def person {:first-name \"Jane\" :last-name \"Doe\" :awesome? true :age 42 :kids [\"Olive\" \"Patterson\"]})"];
    [dl rep:@"(def person-json (encode-json person))"];
    [dl rep:@"(def decoded-person (decode-json person-json))"];
    [dl rep:@"(def keywordized-person (keywordize decoded-person))"];
    XCTAssertEqualObjects([dl rep:@"(= keywordized-person person)"], @"true");
}

- (void)testKeywordize {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    XCTAssertEqualObjects([dl rep:@"(:first-name (keywordize (decode-json \"{\\\"first-name\\\":\\\"Jane\\\",\\\"last-name\\\":\\\"Doe\\\"}\")))"], @"\"Jane\"");
}

- (void)testStringCoreFunctions {
    DreamLisp *dl = [[DreamLisp alloc] initWithoutREPL];
    // uppercase
    XCTAssertEqualObjects([dl rep:@"(uppercase \"I am a teapot\")"], @"\"I AM A TEAPOT\"");
    // lowercase
    XCTAssertEqualObjects([dl rep:@"(lowercase \"LISP IS AWESOME\")"], @"\"lisp is awesome\"");
    // substring
    XCTAssertEqualObjects([dl rep:@"(substring 0 7 \"Run free and dive into the sky\")"], @"\"Run free\"");
    XCTAssertEqualObjects([dl rep:@"(substring 13 29 \"Run free and dive into the sky\")"], @"\"dive into the sky\"");
    // regex match
    XCTAssertEqualObjects([dl rep:@"(def url-regex \"(https|http)(:\\/\\/)(\\D+)(\\.)([a-z]{0,3})(\\/?)\")"],
                          @"\"(https|http)(:\\\\/\\\\/)(\\\\D+)(\\\\.)([a-z]{0,3})(\\\\/?)\"");
    XCTAssertEqualObjects([dl rep:@"(match \"https://dreamlisp.com/\" url-regex)"],
                          @"[[\"https://dreamlisp.com/\" \"https\" \"://\" \"dreamlisp\" \".\" \"com\" \"/\"]]");
    XCTAssertEqualObjects([dl rep:@"(match \"Emily Bronte\" \"[a-zA-Z]+\")"], @"[[\"Emily\"] [\"Bronte\"]]");
    // split
    NSString *toCaps = @"(let [str-xs (split \"init-with-name\" \"-\") s1 (first str-xs) capitalize (fn (s) (let [xs (seq s)] (apply str/1 (cons \
    (uppercase (first xs)) (rest xs)))))] (apply str/1 (cons (capitalize (first str-xs)) (doall (map capitalize/1 (rest str-xs))))))";
    XCTAssertEqualObjects([dl rep:toCaps], @"\"InitWithName\"");
    // trim
    XCTAssertEqualObjects([dl rep:@"(trim \"  nanana batman  \")"], @"\"nanana batman\"");
    // replace
    XCTAssertEqualObjects([dl rep:@"(replace \"123\" \"+\" \"he123llo wo123rld\")"], @"\"he+llo wo+rld\"");
}

- (void)testSplitString {
    NSString *string = @"NSString";
    NSMutableArray *xs = [DLUtils splitString:string];
    XCTAssertEqual([xs count], 8);
    string = @"Pirate Love Daisies";
    xs = [DLUtils splitString:string];
    XCTAssertEqual([xs count], 19);
}

- (void)testStringWithChar {
    NSString *string = @"Pirates";
    unichar uchar = [string characterAtIndex:0];
    NSString *str = [[NSString alloc] initWithCharacters:&uchar length:1];
    XCTAssertTrue([str isEqual:@"P"]);
}

@end
