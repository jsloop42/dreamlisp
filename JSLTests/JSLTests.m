//
//  JSLTests.m
//  JSLTests
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

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
    XCTAssertEqualObjects([s3 value], @"42");
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

- (void)testPrintString {
    Printer *prn = [Printer new];
    // Function
    JSFunction *fn = [[JSFunction alloc] initWithAst:[JSNil new] params:[NSMutableArray new]
                      env:[Env new] macro:false meta:[JSNil new] fn:^id(id arg) { return nil; } name:@"nil-fn/0"];
    XCTAssertEqualObjects([prn printStringFor:fn readably:true], @"nil-fn/0");
    // Symbol
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@"greet"];
    [sym setModuleName:defaultModuleName];
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

- (void)testNSMapTable {
    NSMapTable *table1 = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    [table1 setObject:@"1" forKey:@"1"];
    NSMapTable *table2 = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    [table2 setObject:@"1" forKey:@"1"];
    XCTAssertTrue([table1 isEqual:table2]);
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

void testPrintCallback(id param, int tag, int counter, const char *s) {
    [param printCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] withTag:tag counter:counter];
}

- (void)testPrintFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    infoCallback(self, 0, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println [33 2 3])"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(prn [(+ 21 12) 2 3])"], @"nil");
    freeInfoCallback();
    infoCallback(self, 1, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn)"], @"nil");
    freeInfoCallback();
    infoCallback(self, 2, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn \"\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 3, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 4, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc  def\" \"ghi jkl\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 5, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn \"\\\"\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 6, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc\\ndef\\nghi\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 7, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn \"abc\\\\\\\\def\\\\\\\\ghi\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 8, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(prn (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"nil");
    freeInfoCallback();
    // (println)
    infoCallback(self, 9, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println)"], @"nil");
    freeInfoCallback();
    infoCallback(self, 10, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println \"\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 11, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println \"abc\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 12, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println \"abc  def\" \"ghi jkl\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 13, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println \"\\\"\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 14, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println \"abc\\ndef\\nghi\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 15, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println \"abc\\\\def\\\\ghi\")"], @"nil");
    freeInfoCallback();
    infoCallback(self, 16, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println (list 1 2 \"abc\" \"\\\"\") \"def\")"], @"nil");
    freeInfoCallback();
}

- (void)printCallback:(NSString *)message withTag:(int)tag counter:(int)counter {
    XCTAssertNotNil(message);
    switch (tag) {
        case 0:
            XCTAssertEqualObjects(message, @"[33 2 3]");
            break;
        case 1:
            XCTAssertEqualObjects(message, @"");
            break;
        case 2:
            XCTAssertEqualObjects(message, @"\"\"");
            break;
        case 3:
            XCTAssertEqualObjects(message, @"\"abc\"");
            break;
        case 4:
            XCTAssertEqualObjects(message, @"\"abc  def\" \"ghi jkl\"");
            break;
        case 5:
            XCTAssertEqualObjects(message, @"\"\\\"\"");
            break;
        case 6:
            XCTAssertEqualObjects(message, @"\"abc\\ndef\\nghi\"");
            break;
        case 7:
            XCTAssertEqualObjects(message, @"\"abc\\\\\\\\def\\\\\\\\ghi\"");
            break;
        case 8:
            XCTAssertEqualObjects(message, @"(1 2 \"abc\" \"\\\"\") \"def\"");
            break;
        case 9:
            XCTAssertEqualObjects(message, @"");
            break;
        case 10:
            XCTAssertEqualObjects(message, @"");
            break;
        case 11:
            XCTAssertEqualObjects(message, @"abc");
            break;
        case 12:
            XCTAssertEqualObjects(message, @"abc  def ghi jkl");
            break;
        case 13:
            XCTAssertEqualObjects(message, @"\"");
            break;
        case 14:
            XCTAssertEqualObjects(message, @"abc\ndef\nghi");
            break;
        case 15:
            XCTAssertEqualObjects(message, @"abc\\def\\ghi");
            break;
        case 16:
            XCTAssertEqualObjects(message, @"(1 2 abc \") def");
            break;
        default:
            break;
    }
}

- (void)testDef {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // def! with quote in symbol name
    XCTAssertEqualObjects([jsl rep:@"(def! x 10)"], @"10");
    XCTAssertEqualObjects([jsl rep:@"x"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(def! a' 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"a'"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def! b' '(11 12 13 14))"], @"(11 12 13 14)");
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
    XCTAssertEqualObjects([jsl rep:@"((fn* [] 4) )"], @"4");
    XCTAssertEqualObjects([jsl rep:@"((fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(= [(list)] (list []))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(vector 3 4 5)"], @"[3 4 5]");
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
    XCTAssertEqualObjects([jsl rep:@"(contains? {:abc nil} :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(contains? {:abc 123} :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(get {:abc 123} :abc)"], @"123");
    [jsl rep:@"(def! hm4 (assoc {:a 1 :b 2} :a 3 :c 1))"];
    XCTAssertEqualObjects([jsl rep:@"(get hm4 :a)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(get hm4 :b)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(get hm4 :c)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(hash-map \"a\" 1)"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} \"a\" 1)"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(get (assoc (assoc {\"a\" 1 } \"b\" 2) \"c\" 3) \"a\")"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def! hm1 (hash-map))"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(map? hm1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(map? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(map? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(get nil \"a\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(get hm1 \"a\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(contains? hm1 \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(def! hm2 (assoc hm1 \"a\" 1))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(get hm1 \"a\")"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(contains? hm1 \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(get hm2 \"a\")"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(contains? hm2 \"a\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keys hm1)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(keys hm2)"], @"(\"a\")");
    XCTAssertEqualObjects([jsl rep:@"(vals hm1)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(vals hm2)"], @"(1)");
    XCTAssertEqualObjects([jsl rep:@"(count (keys (assoc hm2 \"b\" 2 \"c\" 3)))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} :bcd 234)"], @"{:bcd 234}");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth (keys {:abc 123 :def 456}) 0))"], @"true");
    [jsl rep:@"(def! hm3 (assoc hm2 \"b\" 2))"];
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth (keys {\":abc\" 123 \":def\" 456}) 0))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth (vals {\"a\" :abc \"b\" :def}) 0))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} :bcd nil)"], @"{:bcd nil}");
    // overwrite duplicate key
    XCTAssertEqualObjects([jsl rep:@"(assoc {:a 1} :a 3)"], @"{:a 3}");
    // testing dissoc
    [jsl rep:@"(def! hm3 (assoc hm2 \"b\" 2))"];
    XCTAssertEqualObjects([jsl rep:@"(count (keys hm3))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(count (vals hm3))"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(dissoc hm3 \"a\")"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc hm3 \"a\" \"b\")"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc hm3 \"a\" \"b\" \"c\")"], @"{}");
    XCTAssertEqualObjects([jsl rep:@"(count (keys hm3))"], @"2");
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
    XCTAssertEqualObjects([jsl rep:@"(def! a nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(assoc {} a 2)"], @"{nil 2}");
    // any key
    XCTAssertEqualObjects([jsl rep:@"{1 1}"], @"{1 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{[\"x\" \"y\"] 1}"], @"{[\"x\" \"y\"] 1}");
    // hash map key evaluation
    XCTAssertEqualObjects([jsl rep:@"(def! a 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"{a 2}"], @"{1 2}");
    XCTAssertThrows([jsl rep:@"{b 2}"], @"'b' not found");
    XCTAssertEqualObjects([jsl rep:@"(contains? {a 2} a)"], @"true");
}

- (void)testEnv {
    Env *env = [Env new];
    [env setModuleName:defaultModuleName];
    JSString *obj = [[JSString alloc] initWithString:@"123"];
    JSSymbol *key = [[JSSymbol alloc] initWithName:@"key"];
    [key setModuleName:defaultModuleName];
    [env setObject:obj forSymbol:key];
    XCTAssertEqualObjects([env objectForSymbol:key], obj);
    Env *aEnv = [[Env alloc] initWithEnv:env];
    JSSymbol *aKey = [[JSSymbol alloc] initWithName:@"aKey"];
    JSString *aObj = [[JSString alloc] initWithString:@"987"];
    [aKey setModuleName:defaultModuleName];
    [aEnv setObject:aObj forSymbol:aKey];
    XCTAssertEqualObjects([aEnv objectForSymbol:aKey], aObj);
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(list? *ARGV*)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"*ARGV*"], @"()");
}

- (void)testSpecialForms {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // def!
    XCTAssertEqualObjects([jsl rep:@"(def! x 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"x"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(def! x 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"x"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(def! y (+ 1 7))"], @"8");
    XCTAssertEqualObjects([jsl rep:@"y"], @"8");
    // case sensitive symbols
    XCTAssertEqualObjects([jsl rep:@"(def! mynum 111)"], @"111");
    XCTAssertEqualObjects([jsl rep:@"(def! MYNUM 222)"], @"222");
    XCTAssertEqualObjects([jsl rep:@"mynum"], @"111");
    XCTAssertEqualObjects([jsl rep:@"MYNUM"], @"222");
    // env lookup error
    XCTAssertEqualObjects([jsl rep:@"(try* (abc 1 2 3) (catch* ex (str ex)))"], @"\"'user:abc/3' not found\"");
    // error aborts def! being re-set
    XCTAssertEqualObjects([jsl rep:@"(def! w 123)"], @"123");
    XCTAssertThrows([jsl rep:@"(def! w (abc))"], @"Symbol not found");
    XCTAssertEqualObjects([jsl rep:@"w"], @"123");
    // let* form
    XCTAssertEqualObjects([jsl rep:@"(let* (z (+ 2 3)) (+ 1 z))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(let* [z 9] z)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(let* (x 9) x)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"x"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(let* (z (+ 2 3)) (+ 1 z))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(let* (p (+ 2 3) q (+ 2 p)) (+ p q))"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def! y (let* (z 7) z))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"y"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(let* (x (or nil \"yes\")) x)"], @"\"yes\"");
    // outer env
    XCTAssertEqualObjects([jsl rep:@"(def! a 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(let* (q 9) q)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(let* (q 9) a)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(let* (z 2) (let* (q 9) a))"], @"4");
    // let* with vector binding
    XCTAssertEqualObjects([jsl rep:@"(let* [z 9] z)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(let* [p (+ 2 3) q (+ 2 p)] (+ p q))"], @"12");
    // vector evaluation
    XCTAssertEqualObjects([jsl rep:@"(let* (a 5 b 6) [3 4 a [b 7] 8])"], @"[3 4 5 [6 7] 8]");
}

- (void)testFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"((fn* (a b) (+ b a)) 3 4)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"((fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(((fn* (a) (fn* (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"((fn* (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"((fn* () 4))"], @"4");
    XCTAssertEqualObjects([jsl rep:@"((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)"], @"8");
    // closure
    XCTAssertEqualObjects([jsl rep:@"(((fn* (a) (fn* (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))"], @"user:gen-plus5/0");
    XCTAssertEqualObjects([jsl rep:@"(def! plus5 (gen-plus5))"], @"user:plus5/1");
    XCTAssertEqualObjects([jsl rep:@"(plus5 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))"], @"user:gen-plusX/1");
    XCTAssertEqualObjects([jsl rep:@"(def! plus7 (gen-plusX 7))"], @"user:plus7/1");
    XCTAssertEqualObjects([jsl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown  (- N 1))) 0)))"], @"user:sumdown/1");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 2)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 6)"], @"21");
    XCTAssertEqualObjects([jsl rep:@"(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"], @"user:fib/1");
    XCTAssertEqualObjects([jsl rep:@"(fib 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(fib 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(fib 4)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) more) 2)"], @"(2)");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (list? more)) 1 2 3)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (count more)) 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (count more)))"], @"0");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (list? more)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"((fn* (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"((fn* (a & more) (count more)) 1)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"((fn* (a & more) (list? more)) 1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply + (list 2 3))"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply + 4 (list 5))"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(apply + 4 [5])"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(apply list [])"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a b) (+ a b)) [2 3])"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a b) (+ a b)) 4 [5])"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (& more) (list? more)) [1 2 3])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (& more) (list? more)) [])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a & more) (list? more)) [1])"], @"true");
    // test bindings
    [jsl rep:@"(def! a (fn* (x) (let* (y x z (* x x)) (+ y z))))"];
    XCTAssertEqualObjects([jsl rep:@"(a 10)"], @"110");
    // test anonymous function print
    XCTAssertEqualObjects([jsl rep:@"(fn* (& more) 1)"], @"#<fn/n>");
    XCTAssertEqualObjects([jsl rep:@"(fn* (a) 1)"], @"#<fn/1>");
    // symbols with quote in name
    XCTAssertEqualObjects([jsl rep:@"(def! c' 10)"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(def! f' (fn* (x) (+ c' x)))"], @"user:f'/1");
    XCTAssertEqualObjects([jsl rep:@"(f' 7)"], @"17");
}

- (void)testMultiArityFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def! a (fn* () 0))"];
    [jsl rep:@"(def! a (fn* (x) 1))"];
    XCTAssertEqualObjects([jsl rep:@"(a)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(a 3)"], @"1");
    // variadic function
    XCTAssertEqualObjects([jsl rep:@"(def! x (fn* (& more) more))"], @"user:x/n");
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
    [jsl rep:@"(def! s (str {:abc \"val1\" :def \"val2\"}))"];
    XCTAssertEqualObjects([jsl rep:@"(or (= s \"{:abc val1 :def val2}\") (= s \"{:def val2 :abc val1}\"))"], @"true");
    NSString *ret = [jsl rep:@"(def! p (pr-str {:abc \"val1\" :def \"val2\"}))"];
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

void testdoPrintCallback(id param, int tag, int counter, const char *s) {
    [param doPrintCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] tag:tag counter:counter];
}

- (void)doPrintCallback:(NSString *)message tag:(int)tag counter:(int)counter {
    switch (tag) {
        case 0:
            XCTAssertEqualObjects(message, @"\"prn output1\"");
            break;
        case 1:
            XCTAssertEqualObjects(message, @"\"prn output2\"");
            break;
        case 2:
            if (counter == 1) {
                XCTAssertEqualObjects(message, @"\"prn output1\"");
            } else if (counter == 2) {
                XCTAssertEqualObjects(message, @"\"prn output2\"");
            }
        default:
            break;
    }
}

- (void)testdoForm {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(do (def! a 6) 7 (+ a 8))"], @"14");
    XCTAssertEqualObjects([jsl rep:@"a"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(def! DO (fn* (a) 7))"], @"user:DO/1");
    XCTAssertEqualObjects([jsl rep:@"(DO 3)"], @"7");
    // printing
    infoCallback(self, 0, &testdoPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(do (prn \"prn output1\"))"], @"nil");
    freeInfoCallback();
    infoCallback(self, 1, &testdoPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(do (prn \"prn output2\") 7)"], @"7");
    freeInfoCallback();
    infoCallback(self, 2, &testdoPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(do (prn \"prn output1\") (prn \"prn output2\") (+ 1 2))"], @"3");
    freeInfoCallback();
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
    XCTAssertEqualObjects([jsl rep:@"(def! a (list 2 3))"], @"(2 3)");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 a)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"a"], @"(2 3)");
    XCTAssertEqualObjects([jsl rep:@"(concat)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(concat (list 1 2))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(concat (list 1 2) (list 3 4))"], @"(1 2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(concat (list 1 2) (list 3 4) (list 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([jsl rep:@"(concat (concat))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(concat (list) (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(def! a (list 1 2))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(def! b (list 3 4))"], @"(3 4)");
    XCTAssertEqualObjects([jsl rep:@"(concat a b (list 5 6))"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([jsl rep:@"a"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"b"], @"(3 4)");
    XCTAssertEqualObjects([jsl rep:@"(cons [1] [2 3])"], @"([1] 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(cons 1 [2 3])"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(concat [1 2] (list 3 4) [5 6])"], @"(1 2 3 4 5 6)");
    XCTAssertEqualObjects([jsl rep:@"(nth (list 1) 0)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(nth (list 1 2) 1)"], @"2");
    [jsl rep:@"(def! x \"x\")"];
    XCTAssertNotEqualObjects(@"(def! x (nth (list 1 2) 2))", @"Index out of bounds");
    XCTAssertEqualObjects([jsl rep:@"x"], @"\"x\"");
    XCTAssertEqualObjects([jsl rep:@"(first (list))"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(first (list 6))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(first (list 7 8 9))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(rest (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(rest (list 6))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(rest (list 7 8 9))"], @"(8 9)");
    XCTAssertEqualObjects([jsl rep:@"(or)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(or 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(or 1 2 3 4)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(or false 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(or false nil 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(or false nil false false nil 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(or false nil 3 false nil 4)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(or (or false 4))"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(cond)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(cond true 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(cond true 7 true 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 true 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 \"else\" 9)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 (= 2 2) 8 \"else\" 9)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(first nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(rest nil)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(conj (list) 1)"], @"(1)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 1) 2)"], @"(2 1)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 2 3) 4)"], @"(4 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 2 3) 4 5 6)"], @"(6 5 4 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(conj (list 1) (list 2 3))"], @"((2 3) 1)");
    XCTAssertEqualObjects([jsl rep:@"(conj '(1 2 3) 4 5 6)"], @"(6 5 4 1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(seq '())"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(seq '(2 3 4))"], @"(2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(seq nil)"], @"nil");
}

- (void)testVectorCoreFunctions {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(map (fn* (a) (* 2 a)) [1 2 3])"], @"(2 4 6)");
    XCTAssertEqualObjects([jsl rep:@"(map (fn* [& args] (list? args)) [1 2])"], @"(true true)");
    XCTAssertEqualObjects([jsl rep:@"(vector? [10 11])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(vector? '(12 13))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(vector 3 4 5)"], @"[3 4 5]");
    XCTAssertEqualObjects([jsl rep:@"(conj [] 1)"], @"[1]");
    XCTAssertEqualObjects([jsl rep:@"(conj [1] 2)"], @"[1 2]");
    XCTAssertEqualObjects([jsl rep:@"(conj [2 3] 4)"], @"[2 3 4]");
    XCTAssertEqualObjects([jsl rep:@"(conj [2 3] 4 5 6)"], @"[2 3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(conj [1] [2 3])"], @"[1 [2 3]]");
    XCTAssertEqualObjects([jsl rep:@"(conj [1 2 3] 4 5 6)"], @"[1 2 3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(nth [1] 0)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(nth [1 2] 1)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(def! x \"x\")"], @"\"x\"");
    XCTAssertThrows([jsl rep:@"(def! x (nth [1 2] 2))"], @"Index out of bounds");
    XCTAssertEqualObjects([jsl rep:@"x"], @"\"x\"");
    XCTAssertEqualObjects([jsl rep:@"(first [])"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(first [10])"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(first [10 11 12])"], @"10");
    XCTAssertEqualObjects([jsl rep:@"(rest [])"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(rest [10])"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(rest [10 11 12])"], @"(11 12)");
    XCTAssertEqualObjects([jsl rep:@"(seq [2 3 4])"], @"(2 3 4)");
    XCTAssertEqualObjects([jsl rep:@"(seq [])"], @"nil");
}

- (void)testKeyword {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(= :abc :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= :abc :def)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= :abc \":abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth (keys {:abc 123 :def 456}) 0))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@":1"], @":1");
    XCTAssertEqualObjects([jsl rep:@"(keyword? :1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(def! a \"abc\")"], @"\"abc\"");
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
    XCTAssertEqualObjects([jsl rep:@"(def! a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (unquote a))"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 a 3))"], @"(1 user:a 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (unquote a) 3))"], @"(1 8 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! b (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 b 3))"], @"(1 user:b 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (unquote b) 3))"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote ((unquote 1) (unquote 2)))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(def! c (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
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
    XCTAssertEqualObjects([jsl rep:@"(def! a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~a 3)"], @"(1 8 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! b '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`(1 b 3)"], @"(1 user:b 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~b 3)"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`(1 c 3)"], @"(1 user:c 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~@c 3)"], @"(1 1 \"b\" \"d\" 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"`[1 a 3]"], @"(1 user:a 3)");
    XCTAssertEqualObjects([jsl rep:@"[1 a 3]"], @"[1 8 3]");
    XCTAssertEqualObjects([jsl rep:@"(def! c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`[1 ~@c 3]"], @"(1 1 \"b\" \"d\" 3)");
}

- (void)testMacro {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmacro! one (fn* () 1))"];
    XCTAssertEqualObjects([jsl rep:@"(one)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(macro? one/0)"], @"true");
    [jsl rep:@"(defmacro! two (fn* () 2))"];
    XCTAssertEqualObjects([jsl rep:@"(two)"], @"2");
    [jsl rep:@"(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))"];
    XCTAssertEqualObjects([jsl rep:@"(unless false 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(unless true 7 8)"], @"8");
    [jsl rep:@"(defmacro! unless2 (fn* (pred a b) `(if (not ~pred) ~a ~b)))"];
    XCTAssertEqualObjects([jsl rep:@"(unless2 false 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(unless2 true 7 8)"], @"8");
    // testing macro expand
    XCTAssertEqualObjects([jsl rep:@"(macroexpand (unless2 2 3 4))"], @"(user:if (core:not 2) 3 4)");
    [jsl rep:@"(defmacro! identity (fn* (x) x))"];
    XCTAssertEqualObjects([jsl rep:@"(let* (a 123) (identity a))"], @"123");
    [jsl rep:@"(defmacro! foo (fn* (& more) (count more)))"];
    XCTAssertEqualObjects([jsl rep:@"(foo 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    // testing gensym
    XCTAssertEqualObjects([jsl rep:@"(= (gensym) (gensym))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(let* [or_FIXME 23] (or false (+ or_FIXME 100)))"], @"123");
    // testing no symbol capture
    [jsl rep:@"(defmacro! pow2 (fn* (a) `(let* (x 2) (* ~a x))))"];
    [jsl rep:@"(def! inc2 (fn* (x) (pow2 x)))"];
    XCTAssertEqualObjects([jsl rep:@"(inc2 5)"], @"10");
    // testing auto gensym reader macro
    [jsl rep:@"(defmacro! pow2* (fn* (a) `(let* (x# 2) (* ~a x#))))"];
    [jsl rep:@"(def! inc2* (fn* (x) (pow2* x)))"];
    XCTAssertEqualObjects([jsl rep:@"(inc2* 5)"], @"10");
    // Testing nested macros
    [jsl rep:@"(defmacro! p1 (fn* (a) `(let* (x 10) (* ~a x))))"];
    [jsl rep:@"(defmacro! p2 (fn* (a) `(let* (x 2) (p1 (* ~a x)))))"];
    [jsl rep:@"(def! n (fn* (x) (p2 x)))"];
    XCTAssertEqualObjects([jsl rep:@"(n 4)"], @"80");
    XCTAssertEqualObjects([jsl rep:@"(n 10)"], @"200");
    [jsl rep:@"(defmacro! p3 (fn* (a) `(let* (x 5) (p2 (* ~a x)))))"];
    [jsl rep:@"(def! n (fn* (x) (p3 x)))"];
    XCTAssertEqualObjects([jsl rep:@"(n 4)"], @"400");
    XCTAssertEqualObjects([jsl rep:@"(n 5)"], @"500");
    XCTAssertEqualObjects([jsl rep:@"`local-sym#"], @"user:local-sym#");  // not gensym reader macro
    // (let* (x#__9__auto__ (1 2 3)) (let* (y#__10__auto__ (first x#__9__auto__)) y#__10__auto__))
    [jsl rep:@"(defmacro! nested-let (fn* () `(let* [x# '(1 2 3)] (let* (y# (first x#)) y#))))"];
    XCTAssertEqualObjects([jsl rep:@"(nested-let)"], @"1");
    // (let* (x__6__auto__ (1 2 3)) (let* (y__11__auto__ (first x__6__auto__)) y__11__auto__))
    [jsl rep:@"(defmacro! nested-let-1 (fn* () `(let* [x '(1 2 3)] (let* (y (first x)) y))))"];
    XCTAssertEqualObjects([jsl rep:@"(nested-let-1)"], @"1");
    // macro with hash-map
    [jsl rep:@"(defmacro! hm (fn* (k v) `(hash-map ~k ~v)))"];
    XCTAssertEqualObjects([jsl rep:@"(hm 1 '(3 4 5))"], @"{1 (3 4 5)}");
    [jsl rep:@"(defmacro! hm1 (fn* (k v) `(let* (x ~v) (hash-map ~k (first x)))))"];
    XCTAssertEqualObjects([jsl rep:@"(hm1 1 '(3 4 5))"], @"{1 3}");
    [jsl rep:@"(defmacro! p (fn* (x) `(let* (z ~x) (list z 4 5 6 7))))"];
    XCTAssertEqualObjects([jsl rep:@"(p 3)"], @"(3 4 5 6 7)");
    XCTAssertEqualObjects([jsl rep:@"(hm 2 (p 3))"], @"{2 (3 4 5 6 7)}");
    XCTAssertEqualObjects([jsl rep:@"(hm1 2 (p 3))"], @"{2 3}");
    [jsl rep:@"(defmacro! p (fn* (x) `(let* (z (atom 3)) (list z 4 5 6 7))))"];
    XCTAssertEqualObjects([jsl rep:@"(hm :b @(first(p 3)))"], @"{:b 3}");
    XCTAssertEqualObjects([jsl rep:@"(hm1 :a (p 5))"], @"{:a (atom 3)}");
    // test macro definition print
    XCTAssertEqualObjects([jsl rep:@"(defmacro! a (fn* (x) `(+ 1 ~x)))"], @"user:a/1");
    XCTAssertEqualObjects([jsl rep:@"(defmacro! a (fn* (& more) `(first (list ~@more))))"], @"user:a/n");
    // misc
    XCTAssertEqualObjects([jsl rep:@"(defun inc (x) (+ x 1))"], @"user:inc/1");
    XCTAssertEqualObjects([jsl rep:@"(defmacro apply1 (x) `(apply inc/1 ~x))"], @"user:apply1/1");
    XCTAssertEqualObjects([jsl rep:@"(apply1 [3])"], @"4");
    // vector binding in let
    XCTAssertEqualObjects([jsl rep:@"(defmacro! m (fn* (x) `(let* (a [1 2 3]) (let* [b [4 5]] (+ ~x (first b))))))"], @"user:m/1");
    XCTAssertEqualObjects([jsl rep:@"(m 3)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo1 () `(let (xs [(atom (fn* (n) (+ n 1))) (atom (fn* (n) (+ n 2)))]) (@(first xs) 10)))"], @"user:foo1/0");
    XCTAssertEqualObjects([jsl rep:@"(foo1)"], @"11");
    XCTAssertEqualObjects([jsl rep:@"(defmacro foo2 () `(let (xs [(atom (fn* (n) (+ n 1))) (atom (fn* (n) (+ n 2)))]) (@(nth xs 1) 10)))"], @"user:foo2/0");
    XCTAssertEqualObjects([jsl rep:@"(foo2)"], @"12");
}

void errorHandleFn(id param, int tag, int counter, const char *s) {
    [param errorHandleCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] withTag:tag counter:counter];
}

- (void)testErrorHandling {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertThrows([jsl rep:@"(throw \"err1\")"], @"Error: err1");
    XCTAssertThrows([jsl rep:@"(throw {:msg \"err2\"})"], @"Error: {:msg \"err2\"}");
    XCTAssertEqualObjects([jsl rep:@"(try* 123 (catch* e 456))"], @"123");
    @try {
        XCTAssertEqualObjects([jsl rep:@"(try* (abc 1 2) (catch* exc (prn \"exc is:\" exc)))"], @"nil");
    } @catch (NSException *exception) {
        XCTAssertEqualObjects([jsl printException:exception log:NO readably:YES], @"exc is:" "'abc' not found");
    }
    XCTAssertEqualObjects([jsl rep:@"(try* (throw \"my exception\") (catch* exc (do (prn \"exc:\" exc) 7)))"], @"7");
    infoCallback(self, 0, &errorHandleFn);
    XCTAssertEqualObjects([jsl rep:@"(try* (abc 1 2) (catch* exc (prn \"exc is:\" exc)))"], @"nil");
    freeInfoCallback();
    infoCallback(self, 1, &errorHandleFn);
    XCTAssertEqualObjects([jsl rep:@"(try* (nth [] 1) (catch* exc (prn \"exc is:\" exc)))"], @"nil");
    freeInfoCallback();
    XCTAssertEqualObjects([jsl rep:@"(try* (map throw/1 (list \"my err\")) (catch* exc exc))"], @"\"my err\"");
    infoCallback(self, 2, &errorHandleFn);
    XCTAssertEqualObjects([jsl rep:@"(try* (throw [\"data\" \"foo\"]) (catch* exc (do (prn \"exc is:\" exc) 7)))"], @"7");
    freeInfoCallback();
    XCTAssertThrows([jsl rep:@"(try* xyz)"], @"'xyz' not found");
    infoCallback(self, 3, &errorHandleFn);
    XCTAssertEqualObjects([jsl rep:@"(try* (throw (list 1 2 3)) (catch* exc (do (prn \"err:\" exc) 7)))"], @"7");
    freeInfoCallback();
}

- (void)errorHandleCallback:(NSString *)message withTag:(int)tag counter:(int)counter {
    XCTAssertNotNil(message);
    switch (tag) {
        case 0:
            XCTAssertEqualObjects(message, @"\"exc is:\" \"'user:abc/2' not found\"");
            break;
        case 1:
            XCTAssertEqualObjects(message, @"\"exc is:\" \"Index 1 is out of bounds of 0\"");
            break;
        case 2:
            XCTAssertEqualObjects(message, @"\"exc is:\" [\"data\" \"foo\"]");
            break;
        case 3:
            XCTAssertEqualObjects(message, @"\"err:\" (1 2 3)");
            break;
    }
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
    XCTAssertEqualObjects([jsl rep:@"(meta (fn* (a) a))"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (fn* (a) a) {\"b\" 1}))"], @"{\"b\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (fn* (a) a) \"abc\"))"], @"\"abc\"");
    [jsl rep:@"(def! l-wm (with-meta (fn* (a) a) {\"b\" 2}))"];
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm/1)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta l-wm/1 {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm/1)"], @"{\"b\" 2}");
    [jsl rep:@"(def! f-wm (with-meta (fn* [a] (+ 1 a)) {\"abc\" 1}))"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm/1)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta f-wm/1 {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm/1)"], @"{\"abc\" 1}");
    [jsl rep:@"(def! f-wm2 ^{\"abc\" 1} (fn* [a] (+ 1 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm2/1)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
    // testing closures and metadata
    [jsl rep:@"(def! gen-plusX (fn* (x) (with-meta (fn* (b) (+ x b)) {\"meta\" 1})))"];
    [jsl rep:@"(def! plus7 (gen-plusX 7))"];
    [jsl rep:@"(def! plus8 (gen-plusX 8))"];
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
    XCTAssertEqualObjects([jsl rep:@"(map? (with-meta {\"abc\" 123} {\"a\" 1}))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta (atom 7) {\"a\" 1}))"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(def! l-wm (with-meta [4 5 6] {\"b\" 2}))"], @"[4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta l-wm {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    // testing metadata on builtin functions
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
    [jsl rep:@"(def! f-wm3 ^{\"def\" 2} +)"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm3/n)"], @"{\"def\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
}

- (void)testTCO {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))"];  // tail recursive
    XCTAssertEqualObjects([jsl rep:@"(sum2 10 0)"], @"55");
    XCTAssertEqualObjects([jsl rep:@"(def! res2 nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(def! res2 (sum2 10000 0))"], @"50005000");
    [jsl rep:@"(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))"];
    [jsl rep:@"(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))"];
    XCTAssertEqualObjects([jsl rep:@"(foo 10000)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(do (do 1 2))"], @"2");
    [jsl rep:@"(def! g (fn* [] 78))"];
    XCTAssertEqualObjects([jsl rep:@"(g)"], @"78");
    [jsl rep:@"(def! g (fn* [a] (+ a 78)))"];
    XCTAssertEqualObjects([jsl rep:@"(g 3)"], @"81");
}

- (NSString *)pathForFile:(NSString *)filename {
    NSFileManager *fm = [NSFileManager defaultManager];
    NSString *path = [[NSString alloc] initWithFormat:@"%@/JSLTests.xctest/Contents/Resources/jsl/%@", [fm currentDirectoryPath], filename];
    if ([fm fileExistsAtPath:path]) {
        return path;
    }
    return @"";
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

- (void)testLoadFile {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    NSString *incPath = [self pathForFile:@"inc.mal"];
    XCTAssertTrue([incPath isNotEmpty]);
    [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", incPath]];
    XCTAssertEqualObjects([jsl rep:@"(inc1 7)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(inc2 7)"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(inc3 9)"], @"12");
    // testing comments
    NSString *incBPath = [self pathForFile:@"incB.mal"];
    XCTAssertTrue([incBPath isNotEmpty]);
    [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", incBPath]];
    XCTAssertEqualObjects([jsl rep:@"(inc4 7)"], @"11");
    XCTAssertEqualObjects([jsl rep:@"(inc5 7)"], @"12");
    // testing map literal across multiple lines in a file
    NSString *incCPath = [self pathForFile:@"incC.mal"];
    XCTAssertTrue([incCPath isNotEmpty]);
    [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", incCPath]];
    XCTAssertEqualObjects([jsl rep:@"mymap"], @"{\"a\" 1}");
}

- (void)testAtom {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def! inc3 (fn* (a) (+ 3 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(def! a (atom 2))"], @"(atom 2)");
    XCTAssertEqualObjects([jsl rep:@"(atom? a)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(atom? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(reset! a 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(swap! a inc3/1)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn* (a) a))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn* (a) (* 2 a)))"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn* (a b) (* a b)) 10)"], @"120");
    XCTAssertEqualObjects([jsl rep:@"(swap! a + 3)"], @"123");
    // testing swap! closure interaction
    [jsl rep:@"(def! inc-it (fn* (a) (+ 1 a)))"];
    [jsl rep:@"(def! atm (atom 7))"];
    [jsl rep:@"(def! f (fn* () (swap! atm inc-it/1)))"];
    XCTAssertEqualObjects([jsl rep:@"(f)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(f)"], @"9");
    // testing `@` deref reader macro
    XCTAssertEqualObjects([jsl rep:@"(def! atm (atom 9))"], @"(atom 9)");
    XCTAssertEqualObjects([jsl rep:@"@atm"], @"9");
    NSString *ret = [jsl rep:@"(def! a (atom {:x 1 :y 2}))"];
    XCTAssertTrue([ret isEqual:@"(atom {:x 1 :y 2})"] || [ret isEqual:@"(atom {:y 2 :x 1})"]);
    XCTAssertEqualObjects([jsl rep:@"(get @a :x)"], @"1");
    ret = [jsl rep:@"(reset! a {:x 1 :y (+ (get @a :y) 1)})"];
    XCTAssertTrue([ret isEqual:@"{:x 1 :y 3}"] || [ret isEqual:@"{:y 3 :x 1}"]);
    XCTAssertEqualObjects([jsl rep:@"(def! a (atom {}))"], @"(atom {})");
    XCTAssertEqualObjects([jsl rep:@"(assoc @a :z 1)"], @"{:z 1}");
    [jsl rep:@"(def! e (atom {\"+\" +}))"];
    [jsl rep:@"(swap! e assoc \"-\" -)"];
    XCTAssertEqualObjects([jsl rep:@"((get @e \"+\") 7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"((get @e \"-\") 11 8)"], @"3");
    [jsl rep:@"(swap! e assoc \"foo\" (list))"];
    XCTAssertEqualObjects([jsl rep:@"(get @e \"foo\")"], @"()");
    [jsl rep:@"(swap! e assoc \"bar\" '(1 2 3))"];
    XCTAssertEqualObjects([jsl rep:@"(get @e \"bar\")"], @"(1 2 3)");
}

- (void)testEval {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // testing eval does not use local environment
    XCTAssertEqualObjects([jsl rep:@"(def! a 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(let* (a 2) (eval (read-string \"a\")))"], @"1");
}

void predicateFn(id param, int tag, int counter, const char *s) {
    [param predicateCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] withTag:tag counter:counter];
}

- (void)testPredicate {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(nil? nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(nil? true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? true)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(true? false)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? true?/1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? :a)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? false)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(false? true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(false? :a)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(apply + (list 2 3))"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply + 4 (list 5))"], @"9");
    infoCallback(self, 0, &predicateFn);
    XCTAssertEqualObjects([jsl rep:@"(apply prn (list 1 2 \"3\" (list)))"], @"nil");
    freeInfoCallback();
    infoCallback(self, 1, &predicateFn);
    XCTAssertEqualObjects([jsl rep:@"(apply prn/n 1 2 (list \"3\" (list)))"], @"nil");
    freeInfoCallback();
    infoCallback(self, 2, &predicateFn);
    XCTAssertEqualObjects([jsl rep:@"(apply prn/n 1 2 [\"3\" 4])"], @"nil");
    freeInfoCallback();
    XCTAssertEqualObjects([jsl rep:@"(apply list (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(apply symbol?/1 (list (quote two)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a b) (+ a b)) (list 2 3))"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a b) (+ a b)) 4 (list 5))"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(def! nums (list 1 2 3))"], @"(1 2 3)");
    [jsl rep:@"(def! double (fn* (a) (* 2 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(double 3)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(map double/1 nums)"], @"(2 4 6)");
    XCTAssertEqualObjects([jsl rep:@"(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))"], @"(false true false)");
    XCTAssertEqualObjects([jsl rep:@"(symbol? :abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? (symbol \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"abc\")"], @"user:abc");
    XCTAssertEqualObjects([jsl rep:@"(keyword :abc)"], @":abc");
    XCTAssertEqualObjects([jsl rep:@"(keyword \"abc\")"], @":abc");
    XCTAssertEqualObjects([jsl rep:@"(sequential? (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(sequential? [15])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(sequential? sequential?/1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(sequential? nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(sequential? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(map? {})"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(map? '())"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(map? [])"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(map? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(map? :abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? \"\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(string? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? \"abc\")"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(string? :abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? (keyword \"abc\"))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? 234)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(string? nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(number? 123)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(number? -1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(number? nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(number? false)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(number? \"123\")"], @"false");
    [jsl rep:@"(def! add1 (fn* (x) (+ x 1)))"];
    XCTAssertEqualObjects([jsl rep:@"(fn? +)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(fn? add1/1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(fn? cond)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(fn? \"+\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(fn? :+)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? cond)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(macro? +)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? add1/1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? \"+\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? :+)"], @"false");
}

- (void)predicateCallback:(NSString *)message withTag:(int)tag counter:(int)counter {
    XCTAssertNotNil(message);
    switch (tag) {
        case 0:
            XCTAssertEqualObjects(message, @"1 2 \"3\" ()");
            break;
        case 1:
            XCTAssertEqualObjects(message, @"1 2 \"3\" ()");
            break;
        case 2:
            XCTAssertEqualObjects(message, @"1 2 \"3\" 4");
            break;
    }
}

- (void)testHygenicSymbols {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(def! n 10) (+ n 1)"];
    [jsl rep:@"(def! x (+ n 5))"];  // n is not gensymed
}

- (void)testMisc {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"nil"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"*host-language*"], @"\"Objective-C 2.0\"");
    [jsl rep:@"(def! start-time (time-ms))"];
    XCTAssertEqualObjects([jsl rep:@"(= start-time 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(let* [sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0))] (sumdown 100)) ; Waste some time"], @"5050");  // not tail recursive
    XCTAssertEqualObjects([jsl rep:@"(> (time-ms) start-time)"], @"true");
}

- (void)testErrorMessages {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    // data type error
    XCTAssertEqualObjects([jsl rep:@"(try* (+ \"\") (catch* ex (str ex)))"], @"\"Expected 'number' but obtained 'string'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (+ []) (catch* ex (str ex)))"], @"\"Expected 'number' but obtained 'vector'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (+ [] \"\") (catch* ex (str ex)))"], @"\"Expected 'number' but obtained 'vector'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (+ '()) (catch* ex (str ex)))"], @"\"Expected 'number' but obtained 'list'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (empty? 1) (catch* ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'number'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (empty? 1.0) (catch* ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'number'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (empty? (atom 1)) (catch* ex (str ex)))"], @"\"'empty?/1' requires 'list' but obtained 'atom'\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (first true) (catch* ex (str ex)))"], @"\"'first/1' requires 'list' or 'vector' for argument 1 but obtained 'bool'\"");
    // Function not found
    XCTAssertEqualObjects([jsl rep:@"(try* (abc [1 2 3]) (catch* ex (str ex)))"], @"\"'user:abc/1' not found\"");
    // Arity error
    XCTAssertEqualObjects([jsl rep:@"(try* (empty? [] []) (catch* ex (str ex)))"], @"\"'user:empty?/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (list? [] []) (catch* ex (str ex)))"], @"\"'user:list?/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(try* (true? [] []) (catch* ex (str ex)))"], @"\"'user:true?/2' not found\"");
    XCTAssertEqualObjects([jsl rep:@"(def! a (atom 4))"], @"(atom 4)");
    XCTAssertEqualObjects([jsl rep:@"(try* (reset! x 4) (catch* ex (str ex)))"], @"\"'user:x' not found\"");
}

/** The expressions loaded and evaluated from file should be added to the env just like it is evaluated from REPL. */
- (void)testScopeWithFile {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    NSString *scopePath = [self pathForFile:@"scope.jsl"];
    XCTAssertTrue([scopePath isNotEmpty]);
    [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", scopePath]];
    XCTAssertEqualObjects([jsl rep:@"(f1 4 6)"], @"30");
}

- (void)testScope {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(def! a 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(def! c 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(def! d (atom 3))"], @"(atom 3)");
    [jsl rep:@"(def! f1 (fn* (a b) (do (+ a b c (deref d)) (let* (x 6 z (+ a c) f (fn* (x) (* x x))) (* x z)))))"];
    XCTAssertEqualObjects([jsl rep:@"(f1 4 6)"], @"30");
}

- (void)testModule {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))"];
    [jsl rep:@"(def! a 1)"];
    XCTAssertEqualObjects([jsl rep:@"a"], @"1");
    [jsl rep:@"(in-module user)"];
}

- (void)testMacroWithModules {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    [jsl rep:@"(defmacro! d (fn* (n v) `(def! ~n ~v)))"];
    XCTAssertEqualObjects([jsl rep:@"(d x 4)"], @"4");
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo ())"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(user:d t 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    [jsl rep:@"(in-module user)"];
    XCTAssertEqualObjects([jsl rep:@"(try* (foo:random-1 4) (catch* ex (str ex)))"], @"\"'foo:random-1/1' not found\"");  // function not exported
}

- (void)testModuleExports {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(defmodule foo (export (inc 1)) (export (dec 1)))"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(defun inc (n) (+ n 1))"], @"foo:inc/1");
    XCTAssertEqualObjects([jsl rep:@"(inc 4)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(defun dec (n) (- n 1))"], @"foo:dec/1");
    XCTAssertEqualObjects([jsl rep:@"(defun greet () 42)"], @"foo:greet/0");
    XCTAssertEqualObjects([jsl rep:@"(greet)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(in-module user)"], @"user");
    XCTAssertEqualObjects([jsl rep:@"(foo:inc 4)"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(try* (foo:random-2) (catch* ex (str ex)))"], @"\"'foo:random-2/0' not found\"");  // function not exported
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
    XCTAssertEqualObjects([jsl rep:@"(in-module user)"], @"user");
    XCTAssertEqualObjects([jsl rep:@"(foo:inc 41)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(foo:dec 43)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(foo:greet)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(defmodule bar (export (sum 2)))"], @"bar");
    XCTAssertEqualObjects([jsl rep:@"(defun sum (x y) (+ x y))"], @"bar:sum/2");
    XCTAssertEqualObjects([jsl rep:@"(defun diff (x y) (- x y))"], @"bar:diff/2");
    XCTAssertEqualObjects([jsl rep:@"(sum 40 2)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(diff 45 3)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(in-module user)"], @"user");
    XCTAssertEqualObjects([jsl rep:@"(bar:sum 40 2)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(try* (bar:diff 45 3) (catch* ex (str ex)))"], @"\"'bar:diff/2' not found\"");  // function not exported
    XCTAssertEqualObjects([jsl rep:@"(in-module foo)"], @"foo");
    XCTAssertEqualObjects([jsl rep:@"(bar:sum 40 2)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(try* (bar:diff 45 3) (catch* ex (str ex)))"], @"\"'bar:diff/2' not found\"");  // function not exported
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
    XCTAssertEqualObjects([jsl rep:@"(in-module user)"], @"user");
    XCTAssertEqualObjects([jsl rep:@"(foo:greet)"], @"42");
    // Invoke MFA from string format
    XCTAssertEqualObjects([jsl rep:@"(eval (read-string \"(foo:greet)\"))"], @"42");
}

- (void)testCoreLib {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    XCTAssertEqualObjects([jsl rep:@"(let (a 11 b (fn* (n) (+ n 1)) c (atom 0)) (reset! c (+ a 10)) (b @c))"], @"22");
}

#pragma mark Env

- (void)testEnvUpdateModuleNameForExprs {
    Env *env = [Env new];
    // Current module name is "user"
    NSString *currModName = @"user";
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@"random-sym-1"];
    // A new symbol before any processing or from current module
    [sym setInitialModuleName:defaultModuleName];
    [sym resetModuleName];
    [sym resetArity];
    [env updateModuleNameForExprs:sym moduleName:currModName];
    XCTAssertEqualObjects([sym moduleName], currModName);
    XCTAssertEqualObjects([sym initialModuleName], currModName);
    // A symbol from core module
    [sym setInitialModuleName:coreModuleName];
    [sym resetModuleName];
    // If a symbol from "core" module is encountered, the module names are reset to core.
    [env updateModuleNameForExprs:sym moduleName:currModName];
    XCTAssertEqualObjects([sym moduleName], coreModuleName);
    XCTAssertEqualObjects([sym initialModuleName], coreModuleName);
    // A symbol from a new module is found
    NSString *newModName = @"io";
    [sym setInitialModuleName:newModName];
    [sym resetModuleName];
    XCTAssertEqualObjects([sym moduleName], newModName);
    XCTAssertEqualObjects([sym initialModuleName], newModName);
    // An symbol with different module names encountered
    [sym setInitialModuleName:defaultModuleName];
    XCTAssertEqualObjects([sym moduleName], newModName);
    XCTAssertEqualObjects([sym initialModuleName], defaultModuleName);
    // An qualified symbol is found
    [sym setIsQualified:YES];
    XCTAssertEqualObjects([sym moduleName], newModName);
    XCTAssertEqualObjects([sym initialModuleName], defaultModuleName);  // No change is made to module names
    // An imported symbol is found
    [sym setIsQualified:NO];
    [sym setIsImported:YES];
    XCTAssertEqualObjects([sym moduleName], newModName);
    XCTAssertEqualObjects([sym initialModuleName], defaultModuleName);  // No change is made to module names
}

- (void)notestCodeLoadedFromFile {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
    NSString *moduleTest = [self pathForFile:@"module-test.jsl"];
    XCTAssertTrue([moduleTest isNotEmpty]);
    [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", moduleTest]];
    XCTAssertEqualObjects([jsl rep:@"(test-in-user)"], @"123");
    XCTAssertEqualObjects([jsl rep:@"(foo:greet-name \"o\")"], @"\"Hello Olivia\"");
    XCTAssertEqualObjects([jsl rep:@"(foo:greet)"], @"\"Hello there\"");
    XCTAssertEqualObjects([jsl rep:@"(foo:fringe-me)"], @"42");
    XCTAssertEqualObjects([jsl rep:@"(foo:magic 42)"], @"\"Now you see me\"");
    XCTAssertEqualObjects([jsl rep:@"(foo:magic 24)"], @"\"Now you don't\"");
    XCTAssertEqualObjects([jsl rep:@"foo:fdefun"], @"foo:fdefun/n");
    XCTAssertEqualObjects([jsl rep:@"(foo:fdefun a () 21)"], @"user:a/0");
    XCTAssertEqualObjects([jsl rep:@"(a)"], @"21");
    XCTAssertEqualObjects([jsl rep:@"(bar:do-magic)"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(bar:magic-magic 42)"], @"1764");
    XCTAssertEqualObjects([jsl rep:@"(bar:bar \"Olive\")"], @"\"I am Olive\"");
}

- (void)test {
    JSL *jsl = [[JSL alloc] initWithoutREPL];
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
