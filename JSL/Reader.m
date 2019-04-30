//
//  Reader.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Reader.h"

@implementation Reader {
    NSMutableArray *_tokens;
    NSUInteger _position;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _tokens = [NSMutableArray new];
        _position = 0;
    }
    return self;
}

- (instancetype)initWithTokens:(NSMutableArray *)array {
    self = [super init];
    if (self) {
        _tokens = array;
        _position = 0;
    }
    return self;
}

- (nullable NSString *)next {
    if (_position >= [_tokens count]) {
        return nil;
    }
    return _tokens[_position++];
}

- (nullable NSString *)peek {
    if (_position >= [_tokens count]) {
        return nil;
    }
    return _tokens[_position];
}

- (void)pass {
    if (_position >= [_tokens count]) {
        return;
    }
    _position++;
}

- (nullable id<JSDataProtocol>)readString:(NSString *)string {
    NSMutableArray *tokens = [self tokenize:string];
    if ([tokens count] <= 0) {
        return nil;
    }
    Reader *reader = [[Reader alloc] initWithTokens:tokens];
    return [reader readForm];
}

- (nullable id<JSDataProtocol>)readForm {
    NSString *token = [self peek];
    if (token == nil) {
        @throw [[NSException alloc] initWithName:JSL_TOKEN_EMPTY reason:JSL_TOKEN_EMPTY_MSG userInfo:nil];
    }
    if ([token isEqual:@"("] || [token isEqual:@"["] || [token isEqual:@"{"]) {
        return [self readListStartingWith:token];
    } else if ([token isEqual:@"'"] || [token isEqual:@"`"] || [token isEqual:@"~"] || [token isEqual:@"~@"] || [token isEqual:@"@"]) {
        NSDictionary *readerMacros = @{@"'": @"quote", @"`": @"quasiquote", @"~": @"unquote", @"~@": @"splice-unquote", @"@": @"deref"};
        [self pass];
        NSArray *ret =  @[[[JSSymbol alloc] initWithName:readerMacros[token]], [self readForm]];
        return [[JSList alloc] initWithArray:ret];
    } else if ([token isEqual:@"^"]) {
        [self pass];
        id<JSDataProtocol> meta = [self readForm];
        return [[JSList alloc] initWithArray: @[[[JSSymbol alloc] initWithName:@"with-meta"], [self readForm], meta]];
    }
    return [self readAtom];
}

- (nullable id<JSDataProtocol>)readListStartingWith:(NSString *)leftParens {
    NSMutableArray *list = [NSMutableArray new];
    NSArray *rightParens = @[@")", @"]", @"}"];
    [self pass];
    while (![rightParens containsObject:[self peek]]) {
        if ([self peek] != nil) {
            [list addObject:[self readForm]];
        } else {
            @throw [[NSException alloc] initWithName:JSL_PARENS_MISMATCH reason:JSL_PARENS_MISMATCH_MSG userInfo:nil];
        }
    }
    if ([leftParens isEqual:@"("] && [[self peek] isEqual:@")"]) {
        [self pass];
        return [[JSList alloc] initWithArray:list];
    } else if ([leftParens isEqual:@"["] && [[self peek] isEqual:@"]"]) {
        [self pass];
        return [[JSVector alloc] initWithArray:list];
    } else if ([leftParens isEqual:@"{"] && [[self peek] isEqual:@"}"]) {
        [self pass];
        return [[JSHashMap alloc] initWithArray:list];
    }
    [self pass];
    return [[JSList alloc] initWithArray:list];
}

- (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern {
    NSRange range = [string rangeOfString:pattern options:NSRegularExpressionSearch range:NSMakeRange(0, [string length])];
    return range.location != NSNotFound;
}

- (nullable id<JSDataProtocol>)readAtom {
    NSString *token = [self next];
    NSString *stringPattern = @"\"(?:\\\\.|[^\\\\\"])*\"";
    NSString *stringUnclosed = @"\"(?:\\\\.|[^\\\\\"])*";
    NSString *numPattern = @"^-?\\d+(\\.\\d+)?$";
    NSString *keywordPattern = @"^:";
    if ([self matchString:token withPattern:numPattern]) {
        return [[JSNumber alloc] initWithString:token];
    } else if ([self matchString:token withPattern:keywordPattern]) {
        return [[JSKeyword alloc] initWithKeyword:token];
    } else if ([self matchString:token withPattern:stringPattern]) {
        NSString *stripped = [token substringWithRange:NSMakeRange(1, [token length] - 2)];
        NSString* ret = [[[[stripped stringByReplacingOccurrencesOfString:@"\\\\" withString:@"\u029e"]
                         stringByReplacingOccurrencesOfString:@"\\\"" withString:@"\""]
                         stringByReplacingOccurrencesOfString:@"\\n" withString:@"\n"]
                         stringByReplacingOccurrencesOfString:@"\u029e" withString:@"\\"];
        return  [[JSString alloc] initWithString:ret];
    } else if ([self matchString:token withPattern:stringUnclosed]) {
        @throw [[NSException alloc] initWithName:JSL_QUOTE_MARK_MISMATCH reason:JSL_QUOTE_MARK_MISMATCH_MSG userInfo:nil];
    } else if ([token isEqual:@"true"]) {
        return [[JSBool alloc] initWithBool:true];
    } else if ([token isEqual:@"false"]) {
        return [[JSBool alloc] initWithBool:false];
    } else if ([token isEqual:@"nil"]) {
        return [JSNil new];
    }
    return [[JSSymbol alloc] initWithName:token];
}

- (NSMutableArray *)tokenize:(NSString *)string {
    NSString *pattern = @"[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\\\].|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)";
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:pattern options:0 error:NULL];
    NSArray *matches = [regex matchesInString:string options:0 range:NSMakeRange(0, [string length])];
    NSMutableArray *tokenArr = [NSMutableArray array];
    for (NSTextCheckingResult *match in matches) {
        NSString * mstr = [string substringWithRange:[match rangeAtIndex:1]];
        if ([mstr characterAtIndex:0] == ';') { continue; }
        [tokenArr addObject:mstr];
    }
    return tokenArr;
}

@end
