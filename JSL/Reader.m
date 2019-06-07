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
    NSString *_stringPattern;
    NSString *_stringUnclosedPattern;
    NSString *_numPattern;
    NSString *_keywordPattern;
    NSString *_tokenPattern;
    NSRegularExpression *_stringExp;
    NSRegularExpression *_stringUnclosedExp;
    NSRegularExpression *_numExp;
    NSRegularExpression *_keywordExp;
    NSRegularExpression *_tokenExp;
    NSString *_moduleName;
    NSArray *_keywords;
}

/** The current token position */
@synthesize position = _position;
@synthesize moduleName = _moduleName;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
        _tokens = [NSMutableArray new];
        _position = 0;
    }
    return self;
}

- (instancetype)initWithTokens:(NSMutableArray *)array moduleName:(NSString *)name {
    self = [super init];
    if (self) {
        [self bootstrap];
        _moduleName = name;
        _tokens = array;
        _position = 0;
    }
    return self;
}

- (void)bootstrap {
    _stringPattern = @"\"(?:\\\\.|[^\\\\\"])*\"";
    _stringUnclosedPattern = @"\"(?:\\\\.|[^\\\\\"])*";
    _numPattern = @"^-?\\d+(\\.\\d+)?$";
    _keywordPattern = @"^:";
    _stringExp = [NSRegularExpression regularExpressionWithPattern:_stringPattern options:0 error:nil];
    _stringUnclosedExp = [NSRegularExpression regularExpressionWithPattern:_stringUnclosedPattern options:0 error:nil];
    _numExp = [NSRegularExpression regularExpressionWithPattern:_numPattern options:0 error:nil];
    _keywordExp = [NSRegularExpression regularExpressionWithPattern:_keywordPattern options:0 error:nil];
    /**
      [\s,]* Matches any number of white spaces or commas. This is not captured, so it will be ignored and not tokenized.
      ~@ Captures the special two-characters ~@ (splice-unquote) and tokenized.
      [\[\]{}()'`~^@] Captures any special single character, one of []{}()'`~^@ and tokenized.
      "(?:\\.|[^\\"])*"? Starts capturing at a double quote and stops at the next double quote unless it was preceded by backslash in which case it includes it
                         until the next double-token and is tokenized. This will also match unbalanced strings with no ending double quote.
     ;.* Captures any sequence of character starting with ; and is tokenized.
     [^\s\[\]{}()"`,;]* Captures a sequence of zero or more non special characters (eg: symbols, numbers, "true", "false" and "nil") and is sort of the inverse
                         of the one above that captures special characters and is tokenized.
     */
    _tokenPattern = @"[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\\\].|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}()\"`,;]+)";
    _tokenExp = [NSRegularExpression regularExpressionWithPattern:_tokenPattern options:0 error:nil];
    _moduleName = [Const defaultModuleName];
    _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*", @"defmodule", @"in-module"];
}

/** Return the next token incrementing the position. */
- (nullable NSString *)next {
    if (_position >= [_tokens count]) return nil;
    return _tokens[_position++];
}

/** Returns the next token without incrementing the position. */
- (nullable NSString *)peek {
    if (_position >= [_tokens count]) return nil;
    return _tokens[_position];
}

/** Increments the token position. */
- (void)pass {
    if (_position >= [_tokens count]) return;
    _position++;
}

/** Converts the string expressions into an array of AST */
- (nullable NSMutableArray<id<JSDataProtocol>> *)readString:(NSString *)string {
    NSMutableArray *tokens = [self tokenize:string];
    if ([tokens count] <= 0) return nil;
    Reader *reader = [[Reader alloc] initWithTokens:tokens moduleName:_moduleName];
    NSMutableArray *exprs = [NSMutableArray new];
    // Evaluate more than one expressions if encountered
    while ([reader position] < [tokens count]) {
        [exprs addObject:[reader readForm]];
    }
    _moduleName = [reader moduleName];
    return exprs;
}

- (nullable id<JSDataProtocol>)readForm {
    NSString *token = [self peek];
    if (token == nil) {
        NSUInteger count = [_tokens count];
        while (_position < count) {
            token = [self peek];
            if (token) break;
        }
        if (_position == count) @throw [[NSException alloc] initWithName:JSL_TOKEN_EMPTY reason:JSL_TOKEN_EMPTY_MSG userInfo:nil];
    }
    if ([token isEqual:@"("] || [token isEqual:@"["] || [token isEqual:@"{"]) {
        return [self readListStartingWith:token];
    } else if ([token isEqual:@"'"] || [token isEqual:@"`"] || [token isEqual:@"~"] || [token isEqual:@"~@"] || [token isEqual:@"@"]) {
        NSDictionary *readerMacros = @{@"'": @"quote", @"`": @"quasiquote", @"~": @"unquote", @"~@": @"splice-unquote", @"@": @"deref"};
        [self pass];
        NSArray *ret =  @[[[JSSymbol alloc] initWithArity:1 position:0 string:readerMacros[token]], [[self readForm] setPosition:1]];
        return [[JSList alloc] initWithArray:ret];
    } else if ([token isEqual:@"^"]) {
        [self pass];
        id<JSDataProtocol> meta = [self readForm];
        return [[JSList alloc] initWithArray: @[[[JSSymbol alloc] initWithArity:2 position:0 string:@"with-meta" moduleName:[Const coreModuleName]],
                                                [[self readForm] setPosition:1], [meta setPosition:2]]];
    }
    return [self readAtom];
}

- (nullable id<JSDataProtocol>)readListStartingWith:(NSString *)leftParens {
    NSMutableArray *list = [NSMutableArray new];
    NSArray *rightParens = @[@")", @"]", @"}"];
    [self pass];
    NSUInteger count = 0;
    while (![rightParens containsObject:[self peek]]) {
        if ([self peek] != nil) {
            [list addObject:[[self readForm] setPosition:count++]];
        } else {
            @throw [[NSException alloc] initWithName:JSL_PARENS_MISMATCH reason:JSL_PARENS_MISMATCH_MSG userInfo:nil];
        }
    }
    if ([leftParens isEqual:@"("] && [[self peek] isEqual:@")"]) {
        [self pass];
        return [[[JSList alloc] initWithArray:list] setPosition:0];
    } else if ([leftParens isEqual:@"["] && [[self peek] isEqual:@"]"]) {
        [self pass];
        return [[[JSVector alloc] initWithArray:list] setPosition:0];
    } else if ([leftParens isEqual:@"{"] && [[self peek] isEqual:@"}"]) {
        [self pass];
        return [[[JSHashMap alloc] initWithArray:list] setPosition:0];
    }
    [self pass];
    return [[[JSList alloc] initWithArray:list] setPosition:0];
}

- (void)symbolParseError:(NSString *)token {
    [[[JSError alloc] initWithFormat:SymbolParseError, token] throw];
}

- (JSSymbol *)symbolFromToken:(NSString *)token {
    if ([token isEqual:@"/"]) {
        return [[JSSymbol alloc] initWithArity:-1 string:@"/" moduleName:[Const coreModuleName]];
    }
    if ([token isEqual:@"defmodule"] || [token isEqual:@"in-module"]) {
        _moduleName = [self peek];
    }
    NSArray *modArr = [token componentsSeparatedByString:@":"];
    NSUInteger modCount = [modArr count];
    if (modCount > 2) [self symbolParseError:token];
    if (modCount == 2) token = modArr[1];  // the function part
    NSArray *symArr = [token componentsSeparatedByString:@"/"];
    NSUInteger count = [symArr count];
    NSString *arity = nil;
    JSSymbol *sym = nil;
    if (count > 2) [self symbolParseError:token];
    if (count == 2) {
        arity = symArr[1];
        if ([arity isNotEmpty]) {
            sym = [[JSSymbol alloc] initWithArity:[arity isEqual:@"n"] ? -1 : [arity integerValue] string:symArr[0]];
        } else {
            [self symbolParseError:token];
        }
    } else if (count == 1) {
        sym = [[JSSymbol alloc] initWithName:token];
    }
    [sym setInitialModuleName:_moduleName];
    [sym resetModuleName];
    // Fully qualified symbol with module name included
    if (modCount == 2) {
        [sym setIsQualified:YES];
        [sym setInitialModuleName:modArr[0]];
    }
    return sym;
}

/** Read individual elements. */
- (nullable id<JSDataProtocol>)readAtom {
    NSString *token = [self next];
    if ([Utils matchString:token withExpression:_numExp]) {
        return [[JSNumber alloc] initWithString:token];
    } else if ([Utils matchString:token withExpression:_keywordExp]) {
        return [[JSKeyword alloc] initWithKeyword:token];
    } else if ([Utils matchString:token withExpression:_stringExp]) {
        NSString *stripped = [token substringWithRange:NSMakeRange(1, [token length] - 2)];
        NSString* ret = [[[[stripped stringByReplacingOccurrencesOfString:@"\\\\" withString:@"\u029e"]
                         stringByReplacingOccurrencesOfString:@"\\\"" withString:@"\""]
                         stringByReplacingOccurrencesOfString:@"\\n" withString:@"\n"]
                         stringByReplacingOccurrencesOfString:@"\u029e" withString:@"\\"];
        return  [[JSString alloc] initWithString:ret];
    } else if ([Utils matchString:token withExpression:_stringUnclosedExp]) {
        @throw [[NSException alloc] initWithName:JSL_QUOTE_MARK_MISMATCH reason:JSL_QUOTE_MARK_MISMATCH_MSG userInfo:nil];
    } else if ([token isEqual:@"true"]) {
        return [[JSBool alloc] initWithBool:true];
    } else if ([token isEqual:@"false"]) {
        return [[JSBool alloc] initWithBool:false];
    } else if ([token isEqual:@"nil"]) {
        return [JSNil new];
    }
    return [self symbolFromToken:token];
}

- (NSMutableArray *)tokenize:(NSString *)string {
    NSArray *matches = [_tokenExp matchesInString:string options:0 range:NSMakeRange(0, [string length])];
    NSMutableArray *tokenArr = [NSMutableArray array];
    for (NSTextCheckingResult *match in matches) {
        NSString * mstr = [string substringWithRange:[match rangeAtIndex:1]];
        if ([mstr characterAtIndex:0] == ';') continue;
        [tokenArr addObject:mstr];
    }
    return tokenArr;
}

@end
