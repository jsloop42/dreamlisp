//
//  Const.m
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Const.h"

NSString * const JSLVersion = @"3.2";
static NSArray *_keywords;
static NSString * _defaultModuleName;
static NSString * _coreModuleName;

@implementation Const

+ (void)initialize {
    if (self == [self class]) {
        _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*", @"defmodule",
                      @"in-module", @"import", @"export", @"remove-module"];
        _defaultModuleName = @"user";
        _coreModuleName = @"core";
    }
}

+ (NSArray *)keyword {
    return _keywords;
}

+ (NSString *)defaultModuleName {
    return _defaultModuleName;
}

+ (NSString *)coreModuleName {
    return _coreModuleName;
}

@end


#pragma mark Error constant

NSString * const JSL_ERROR_TYPE = @"Type error";
NSString * const JSL_PARENS_MISMATCH = @"Parenthesis mismatch";
NSString * const JSL_QUOTE_MARK_MISMATCH = @"Quote mark mismatch";
NSString * const JSL_TOKEN_EMPTY = @"Empty token";
NSString * const JSL_SYMBOL_NOT_FOUND = @"Symbol not found";
NSString * const JSL_INVALID_ARGUMENT = @"Invalid argument";
NSString * const JSL_INDEX_OUT_OF_BOUNDS = @"Index out of bounds";
NSString * const JSL_FILE_READ_ERROR = @"File read error";

#pragma mark Error message

NSString * const JSL_ERROR_TYPE_MSG = @"Error type";
NSString * const JSL_PARENS_MISMATCH_MSG = @"Parenthesis unbalanced";
NSString * const JSL_QUOTE_MARK_MISMATCH_MSG = @"Quotation mark unbalanced";
NSString * const JSL_TOKEN_EMPTY_MSG = @"Token is empty";
NSString * const JSL_SYMBOL_NOT_FOUND_MSG = @"Symbol not found";
NSString * const JSL_INVALID_ARGUMENT_MSG = @"Invalid argument";
NSString * const JSL_INDEX_OUT_OF_BOUNDS_MSG = @"Index out of bounds";
NSString * const JSL_FILE_READ_ERROR_MSG = @"File read error";
