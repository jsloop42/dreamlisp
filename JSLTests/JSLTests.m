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
    [param printCallback:[[NSString alloc] initWithCString:s encoding:NSUTF8StringEncoding]];
}

- (void)testPrintFunctions {
    JSL *jsl = [JSL new];
    infoCallback(self, 0, &testPrintCallback);
    XCTAssertEqualObjects([jsl rep:@"(println [33 2 3])"], @"nil");
    XCTAssertEqualObjects([jsl rep:@"(prn [(+ 21 12) 2 3])"], @"nil");
    freeInfoCallback();
    XCTAssertEqualObjects([jsl rep:@"(pr-str [(+ 21 12) 2 3])"], @"\"[33 2 3]\"");
    XCTAssertEqualObjects([jsl rep:@"(str [(+ 21 12) 2 3])"], @"\"[33 2 3]\"");
    XCTAssertEqualObjects([jsl rep:@"(str [(+ 21 12) 2 3 \"foo\"])"], @"\"[33 2 3 foo]\"");
}

- (void)printCallback:(NSString *)message {
    XCTAssertNotNil(message);
    XCTAssertEqualObjects(message, @"[33 2 3]");
}

- (void)testList {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"()"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(list)"], @"()");
    XCTAssertEqualObjects([jsl rep:@"(list 1 2 3)"], @"(1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(list 1 (list 21 22 23) 3)"], @"(1 (21 22 23) 3)");
}

- (void)testHashMap {
    JSL *jsl = [JSL new];
    XCTAssertEqualObjects([jsl rep:@"{\"a\" 1}"], @"{\"a\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"abc\" 1}"], @"{\"abc\" 1}");
    XCTAssertEqualObjects([jsl rep:@"{\"a\" (+ 1 2)}"], @"{\"a\" 3}");
    XCTAssertEqualObjects([jsl rep:@"{:a (+ 7 8)}"], @"{:a 15}");
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
    XCTAssertEqualObjects([jsl rep:@"(conj [1 2 3] 4 5 6)"], @"[1 2 3 4 5 6]");
    XCTAssertEqualObjects([jsl rep:@"(conj '(1 2 3) 4 5 6)"], @"(6 5 4 1 2 3)");
    XCTAssertEqualObjects([jsl rep:@"(empty? (list))"], @"true");
    XCTAssertEqualObjects([jsl rep:@"(empty? (list 1))"], @"false");
    XCTAssertEqualObjects([jsl rep:@"(count (list 1 2 3))"], @"3");
    XCTAssertEqualObjects([jsl rep:@"(count (list))"], @"0");
    XCTAssertEqualObjects([jsl rep:@"(count nil)"], @"0");
}

- (void)testMisc {

}

// TODO: add test cases from mal

- (void)test {
//    JSL *jsl = [JSL new];

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
