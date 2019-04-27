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
    XCTAssertFalse([JSKeyword isKeyword:[JSNumber new]]);
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
    JSFunction *fn = [[JSFunction alloc] initWithAst:[JSNil new] params:[NSMutableArray new] env:[Env new] macro:false meta:[JSNil new]
                                                  fn:^id(id arg){return nil;}];
    XCTAssertEqualObjects([prn printStringFor:fn readably:true], @"#<function>");
    // Symbol
    JSSymbol *sym = [[JSSymbol alloc] initWithName:@"greet"];
    XCTAssertEqualObjects([prn printStringFor:sym readably:true], @"greet");
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

- (void)testJSHashMap {
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
    JSData * aRet = [aDict objectForKey:aKey];
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
    JSL *jsl = [JSL new];
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
}

- (void)testComparisonFunctions {
    JSL *jsl = [JSL new];
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
}

void testPrintCallback(id param, int tag, int counter, const char *s) {
    [param printCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] withTag:tag counter:counter];
}

- (void)testPrintFunctions {
    JSL *jsl = [JSL new];
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

- (void)testList {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"()"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(list)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(list 1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(list 1 (list 21 22 23) 3)"], @"(1 (21 22 23) 3)");
    XCTAssertEqualObjects([jsl rep:@"(first nil)"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(rest nil)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(first [])"], @"nil");
}

- (void)testVector {
    JSL *jsl = [JSL new];
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
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"abc\" 1}"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" (+ 1 2)}"], @"{\"a\" 3}");
    XCTAssertEqualObjects([jsl rep:@"{:a (+ 7 8)}"], @"{:a 15}");
    XCTAssertEqualObjects([jsl rep:@"(dissoc {:a 1 :b 2} :a)"], @"{:b 2}");
    XCTAssertEqualObjects([jsl rep:@"(keys {:abc 123 :def 456})"], @"(:abc :def)");
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
}

- (void)testEnv {
    Env *env = [Env new];
    JSString *obj = [[JSString alloc] initWithString:@"123"];
    JSSymbol *key = [[JSSymbol alloc] initWithName:@"key"];
    [env setObject:obj forSymbol:key];
    XCTAssertEqualObjects([env objectForSymbol:key], obj);
    Env *aEnv = [[Env alloc] initWithEnv:env];
    JSSymbol *aKey = [[JSSymbol alloc] initWithName:@"aKey"];
    JSString *aObj = [[JSString alloc] initWithString:@"987"];
    [aEnv setObject:aObj forSymbol:aKey];
    XCTAssertEqualObjects([aEnv objectForSymbol:aKey], aObj);
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"(list? *ARGV*)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"*ARGV*"], @"()");
}

- (void)testSpecialForms {
    JSL *jsl = [JSL new];
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
    @try {
        XCTAssertThrowsSpecificNamed([jsl rep:@"(abc 1 2 3)"], NSException, JSL_SYMBOL_NOT_FOUND, @"Symbol not found");
    } @catch (NSException *exception) {
        XCTAssertTrue([Utils matchString:[exception.userInfo objectForKey:@"description"] withPattern:@".*\\'?abc\\'? not found.*"]);
    }
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
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"((fn* (a b) (+ b a)) 3 4)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"((fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(((fn* (a) (fn* (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"((fn* (& more) (count more)) 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"((fn* (a & more) (count more)) 1 2 3)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"((fn* () 4))"], @"4");
    XCTAssertEqualObjects([jsl rep:@"((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)"], @"8");
    // closure
    XCTAssertEqualObjects([jsl rep:@"(((fn* (a) (fn* (b) (+ a b))) 5) 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))"], @"#<function>");
    XCTAssertEqualObjects([jsl rep:@"(def! plus5 (gen-plus5))"], @"#<function>");
    XCTAssertEqualObjects([jsl rep:@"(plus5 7)"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))"], @"#<function>");
    XCTAssertEqualObjects([jsl rep:@"(def! plus7 (gen-plusX 7))"], @"#<function>");
    XCTAssertEqualObjects([jsl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown  (- N 1))) 0)))"], @"#<function>");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 2)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(sumdown 6)"], @"21");
    XCTAssertEqualObjects([jsl rep:@"(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))"], @"#<function>");
    XCTAssertEqualObjects([jsl rep:@"(fib 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(fib 2)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(fib 4)"], @"5");
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
}

- (void)testNotFunction {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"(not false)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(not true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not \"a\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not 0)"], @"false");
}

- (void)testString {
    JSL *jsl = [JSL new];
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
    JSL *jsl = [JSL new];
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
    XCTAssertEqualObjects([jsl rep:@"(str true \".\" false \".\" nil \".\" :keyw \".\" 'symb)"], @"\"true.false.nil.:keyw.symb\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str \"A\" {:abc \"val\"} \"Z\")"], @"\"\\\"A\\\" {:abc \\\"val\\\"} \\\"Z\\\"\"");
    XCTAssertEqualObjects([jsl rep:@"(pr-str true \".\" false \".\" nil \".\" :keyw \".\" 'symb)"],
                          @"\"true \\\".\\\" false \\\".\\\" nil \\\".\\\" :keyw \\\".\\\" symb\"");
    [jsl rep:@"(def! s (str {:abc \"val1\" :def \"val2\"}))"];
    XCTAssertEqualObjects([jsl rep:@"(or (= s \"{:abc val1 :def val2}\") (= s \"{:def val2 :abc val1}\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(def! p (pr-str {:abc \"val1\" :def \"val2\"}))"], @"\"{:abc \\\"val1\\\" :def \\\"val2\\\"}\"");
    XCTAssertEqualObjects([jsl rep:@"(or (= p \"{:abc \\\"val1\\\" :def \\\"val2\\\"}\") (= p \"{:def \\\"val2\\\" :abc \\\"val1\\\"}\"))"], @"true");
}

- (void)testStringFunctions {
    JSL *jsl = [JSL new];
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
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"(do (def! a 6) 7 (+ a 8))"], @"14");
    XCTAssertEqualObjects([jsl rep:@"a"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(def! DO (fn* (a) 7))"], @"#<function>");
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
    JSL *jsl = [JSL new];
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
    XCTAssertEqualObjects([jsl rep:@"(= \"abc\" (str (quote abc)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= (quote abc) nil)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= nil (quote abc))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not (= 1 1))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(not (= 1 2))"], @"true");
}

- (void)testConditional {
    JSL *jsl = [JSL new];
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
    JSL *jsl = [JSL new];
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
    NSString *corePath = [self pathForFile:@"core.mal"];
    XCTAssertTrue([corePath isNotEmpty]);
    [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", corePath]];
    XCTAssertEqualObjects([jsl rep:@"(-> 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(-> (list 7 8 9) first)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(-> (list 7 8 9) (first))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(-> (list 7 8 9) first (+ 7))"], @"14");
    XCTAssertEqualObjects([jsl rep:@"(-> (list 7 8 9) rest (rest) first (+ 7))"], @"16");
    XCTAssertEqualObjects([jsl rep:@"(->> \"L\")"], @"\"L\"");
    XCTAssertEqualObjects([jsl rep:@"(->> \"L\" (str \"A\") (str \"M\"))"], @"\"MAL\"");
    XCTAssertEqualObjects([jsl rep:@"(->> [4] (concat [3]) (concat [2]) rest (concat [1]))"], @"(1 3 4)");
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
    JSL *jsl = [JSL new];
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
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"(= :abc :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(= :abc :def)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(= :abc \":abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (nth (keys {:abc 123 :def 456}) 0))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"\")"], @"false");
}

- (void)testQuote {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"(quote 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(quote (1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(quote (1 2 (3 4)))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote 7)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 2 3))"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 2 (3 4)))"], @"(1 2 (3 4))");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (nil))"], @"(nil)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (unquote 7))"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(def! a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote a)"], @"a");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (unquote a))"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 a 3))"], @"(1 a 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (unquote a) 3))"], @"(1 8 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! b (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 b 3))"], @"(1 b 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 (unquote b) 3))"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote ((unquote 1) (unquote 2)))"], @"(1 2)");
    XCTAssertEqualObjects([jsl rep:@"(def! c (quote (1 \"b\" \"d\")))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"(quasiquote (1 c 3))"], @"(1 c 3)");
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
    XCTAssertEqualObjects([jsl rep:@"`(1 b 3)"], @"(1 b 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~b 3)"], @"(1 (1 \"b\" \"d\") 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`(1 c 3)"], @"(1 c 3)");
    XCTAssertEqualObjects([jsl rep:@"`(1 ~@c 3)"], @"(1 1 \"b\" \"d\" 3)");
    XCTAssertEqualObjects([jsl rep:@"(def! a 8)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"`[1 a 3]"], @"(1 a 3)");
    XCTAssertEqualObjects([jsl rep:@"[1 a 3]"], @"[1 8 3]");
    XCTAssertEqualObjects([jsl rep:@"(def! c '(1 \"b\" \"d\"))"], @"(1 \"b\" \"d\")");
    XCTAssertEqualObjects([jsl rep:@"`[1 ~@c 3]"], @"(1 1 \"b\" \"d\" 3)");
}

- (void)testQuine {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects(
        [jsl rep:@"((fn* [q] (quasiquote ((unquote q) (quote (unquote q))))) (quote (fn* [q] (quasiquote ((unquote q) (quote (unquote q)))))))"],
                 @"((fn* [q] (quasiquote ((unquote q) (quote (unquote q))))) (quote (fn* [q] (quasiquote ((unquote q) (quote (unquote q)))))))");
}

- (void)testMacro {
    JSL *jsl = [JSL new];
    [jsl rep:@"(defmacro! one (fn* () 1))"];
    XCTAssertEqualObjects([jsl rep:@"(one)"], @"1");
    [jsl rep:@"(defmacro! two (fn* () 2))"];
    XCTAssertEqualObjects([jsl rep:@"(two)"], @"2");
    [jsl rep:@"(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))"];
    XCTAssertEqualObjects([jsl rep:@"(unless false 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(unless true 7 8)"], @"8");
    [jsl rep:@"(defmacro! unless2 (fn* (pred a b) `(if (not ~pred) ~a ~b)))"];
    XCTAssertEqualObjects([jsl rep:@"(unless2 false 7 8)"], @"7");
    XCTAssertEqualObjects([jsl rep:@"(unless2 true 7 8)"], @"8");
    // testing macro expand
    XCTAssertEqualObjects([jsl rep:@"(macroexpand (unless2 2 3 4))"], @"(if (not 2) 3 4)");
    [jsl rep:@"(defmacro! identity (fn* (x) x))"];
    XCTAssertEqualObjects([jsl rep:@"(let* (a 123) (identity a))"], @"123");
    [jsl rep:@"(defmacro! foo (fn* (& more) (count more)))"];
    XCTAssertEqualObjects([jsl rep:@"(foo 1 2 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(cond false 7 false 8 false 9)"], @"nil");
    // testing gensym
    XCTAssertEqualObjects([jsl rep:@"(= (gensym) (gensym))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(let* [or_FIXME 23] (or false (+ or_FIXME 100)))"], @"123");
}

void errorHandleFn(id param, int tag, int counter, const char *s) {
    [param errorHandleCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] withTag:tag counter:counter];
}

- (void)testErrorHandling {
    JSL *jsl = [JSL new];
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
    XCTAssertEqualObjects([jsl rep:@"(try* (map throw (list \"my err\")) (catch* exc exc))"], @"\"my err\"");
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
            XCTAssertEqualObjects(message, @"\"exc is:\" \"'abc' not found\"");
            break;
        case 1:
            XCTAssertEqualObjects(message, @"\"exc is:\" \"Index out of bounds\"");
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
    JSL *jsl = [JSL new];
    JSSymbol *sym = [JSSymbol new];
    XCTAssertFalse([sym hasMeta]);
    JSData *meta = [[JSString alloc] initWithString:@"meta-string"];
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
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta l-wm {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta l-wm)"], @"{\"b\" 2}");
    [jsl rep:@"(def! f-wm (with-meta (fn* [a] (+ 1 a)) {\"abc\" 1}))"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta f-wm {\"new_meta\" 123}))"], @"{\"new_meta\" 123}");
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm)"], @"{\"abc\" 1}");
    [jsl rep:@"(def! f-wm2 ^{\"abc\" 1} (fn* [a] (+ 1 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm2)"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
    // testing closures and metadata
    [jsl rep:@"(def! gen-plusX (fn* (x) (with-meta (fn* (b) (+ x b)) {\"meta\" 1})))"];
    [jsl rep:@"(def! plus7 (gen-plusX 7))"];
    [jsl rep:@"(def! plus8 (gen-plusX 8))"];
    XCTAssertEqualObjects([jsl rep:@"(plus7 8)"], @"15");
    XCTAssertEqualObjects([jsl rep:@"(meta plus7)"], @"{\"meta\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta plus8)"], @"{\"meta\" 1}");
    XCTAssertEqualObjects([jsl rep:@"(meta (with-meta plus7 {\"meta\" 2}))"], @"{\"meta\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta plus8)"], @"{\"meta\" 1}");
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
    XCTAssertEqualObjects([jsl rep:@"(meta f-wm3)"], @"{\"def\" 2}");
    XCTAssertEqualObjects([jsl rep:@"(meta +)"], @"nil");
}

- (void)testTCO {
    JSL *jsl = [JSL new];
    [jsl rep:@"(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))"];
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
    NSString *path = [[NSString alloc] initWithFormat:@"%@/JSLTests.xctest/Contents/Resources/tests/%@", [fm currentDirectoryPath], filename];
    if ([fm fileExistsAtPath:path]) {
        return path;
    }
    return @"";
}

- (void)testReadString {
    JSL *jsl = [JSL new];
    FileOps *fops = [FileOps new];
    XCTAssertEqualObjects([jsl rep:@"(read-string \"(1 2 (3 4) nil)\")"], @"(1 2 (3 4) nil)");
    XCTAssertEqualObjects([jsl rep:@"(read-string \"(+ 2 3)\")"], @"(+ 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(read-string \"7 ;; comment\")"], @"7");
    XCTAssertNil([jsl rep:@"(read-string \";; comment\")"]);
    XCTAssertEqualObjects([jsl rep:@"(eval (read-string \"(+ 2 3)\"))"], @"5");
    [fops createFileIfNotExist:@"/tmp/jsl-test.txt"];
    [fops append:@"A line of text\n" completion: ^{
        XCTAssertEqualObjects([jsl rep:@"(slurp \"/tmp/jsl-test.txt\")"], @"\"A line of text\\n\"");
        [fops closeFile];
        [fops delete:@"/tmp/jsl-test.txt"];
    }];
    XCTAssertNil([jsl rep:@"(read-string \";\")"]);
}

- (void)testLoadFile {
    JSL *jsl = [JSL new];
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
    JSL *jsl = [JSL new];
    [jsl rep:@"(def! inc3 (fn* (a) (+ 3 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(def! a (atom 2))"], @"(atom 2)");
    XCTAssertEqualObjects([jsl rep:@"(atom? a)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(atom? 1)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"2");
    XCTAssertEqualObjects([jsl rep:@"(reset! a 3)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(swap! a inc3)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(deref a)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn* (a) a))"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn* (a) (* 2 a)))"], @"12");
    XCTAssertEqualObjects([jsl rep:@"(swap! a (fn* (a b) (* a b)) 10)"], @"120");
    XCTAssertEqualObjects([jsl rep:@"(swap! a + 3)"], @"123");
    // testing swap! closure interaction
    [jsl rep:@"(def! inc-it (fn* (a) (+ 1 a)))"];
    [jsl rep:@"(def! atm (atom 7))"];
    [jsl rep:@"(def! f (fn* () (swap! atm inc-it)))"];
    XCTAssertEqualObjects([jsl rep:@"(f)"], @"8");
    XCTAssertEqualObjects([jsl rep:@"(f)"], @"9");
    // testing `@` deref reader macro
    XCTAssertEqualObjects([jsl rep:@"(def! atm (atom 9))"], @"(atom 9)");
    XCTAssertEqualObjects([jsl rep:@"@atm"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(def! a (atom {:x 1 :y 2}))"], @"(atom {:x 1 :y 2})");
    XCTAssertEqualObjects([jsl rep:@"(get @a :x)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(reset! a {:x 1 :y (+ (get @a :y) 1)})"], @"{:x 1 :y 3}");
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
    JSL *jsl = [JSL new];
    // testing eval does not use local environment
    XCTAssertEqualObjects([jsl rep:@"(def! a 1)"], @"1");
    XCTAssertEqualObjects([jsl rep:@"(let* (a 2) (eval (read-string \"a\")))"], @"1");
}

void predicateFn(id param, int tag, int counter, const char *s) {
    [param predicateCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding] withTag:tag counter:counter];
}

- (void)testPredicate {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(nil? nil)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(nil? true)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? true)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(true? false)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(true? true?)"], @"false");
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
    XCTAssertEqualObjects([jsl rep:@"(apply prn 1 2 (list \"3\" (list)))"], @"nil");
    freeInfoCallback();
    infoCallback(self, 2, &predicateFn);
    XCTAssertEqualObjects([jsl rep:@"(apply prn 1 2 [\"3\" 4])"], @"nil");
    freeInfoCallback();
    XCTAssertEqualObjects([jsl rep:@"(apply list (list))"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(apply symbol? (list (quote two)))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a b) (+ a b)) (list 2 3))"], @"5");
    XCTAssertEqualObjects([jsl rep:@"(apply (fn* (a b) (+ a b)) 4 (list 5))"], @"9");
    XCTAssertEqualObjects([jsl rep:@"(def! nums (list 1 2 3))"], @"(1 2 3)");
    [jsl rep:@"(def! double (fn* (a) (* 2 a)))"];
    XCTAssertEqualObjects([jsl rep:@"(double 3)"], @"6");
    XCTAssertEqualObjects([jsl rep:@"(map double nums)"], @"(2 4 6)");
    XCTAssertEqualObjects([jsl rep:@"(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))"], @"(false true false)");
    XCTAssertEqualObjects([jsl rep:@"(symbol? :abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? 'abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol? \"abc\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(symbol? (symbol \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? :abc)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(keyword? 'abc)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? \"\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(keyword? (keyword \"abc\"))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(symbol \"abc\")"], @"abc");
    XCTAssertEqualObjects([jsl rep:@"(keyword :abc)"], @":abc");
    XCTAssertEqualObjects([jsl rep:@"(keyword \"abc\")"], @":abc");
    XCTAssertEqualObjects([jsl rep:@"(sequential? (list 1 2 3))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(sequential? [15])"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(sequential? sequential?)"], @"false");
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
    XCTAssertEqualObjects([jsl rep:@"(fn? add1)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(fn? cond)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(fn? \"+\")"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(fn? :+)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? cond)"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(macro? +)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(macro? add1)"], @"false");
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

- (void)testMisc {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"nil"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"*host-language*"], @"\"Objective-C 2.0\"");
    [jsl rep:@"(def! start-time (time-ms))"];
    XCTAssertEqualObjects([jsl rep:@"(= start-time 0)"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(let* [sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0))] (sumdown 10)) ; Waste some time"], @"55");
    XCTAssertEqualObjects([jsl rep:@"(> (time-ms) start-time)"], @"true");
}

- (void)test {
    JSL *jsl = [JSL new];
//    XCTAssertEqualObjects([jsl rep:@"{1 2}"], @"{1 2}");
//    XCTAssertEqualObjects([jsl rep:@"{1 1}"], @"{1 1}");
//    NSMutableDictionary *dict = [NSMutableDictionary new];
//    JSString *val = [[JSString alloc] initWithString:@"val"];
//    JSSymbol *key = [[JSSymbol alloc] initWithName:@"sym"];
//    [dict setObject:val forKey:key];
//    JSData * ret = [dict objectForKey:key];
//    XCTAssertNotNil(ret);
//    XCTAssertEqualObjects([ret dataType], @"JSString");

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
