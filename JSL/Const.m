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
static NSString *_emptyModuleName;
static NSString *_defaultModuleName;
static NSString *_defaultModuleDescription;
static NSString *_coreModuleName;
static NSString *_exports;
static NSString *_imports;
static NSString *_internal;
static NSString *_name;
static NSString *_description;
static NSString *_ascendingKeyword;
static NSString *_descendingKeyword;
static NSString *_keyKeyword;
static NSString *_valueKeyword;

@implementation Const

+ (void)initialize {
    if (self == [self class]) {
        _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*", @"defmodule",
                      @"in-module", @"import", @"export", @"remove-module"];
        _emptyModuleName = @"*";
        _defaultModuleName = @"user";
        _defaultModuleDescription = @"The default module.";
        _coreModuleName = @"core";
        _exports = @"exports";
        _imports = @"imports";
        _internal = @"internal";
        _name = @"name";
        _description = @"description";
        _ascendingKeyword = @":asc";
        _descendingKeyword = @":desc";
        _keyKeyword = @":key";
        _valueKeyword = @":value";
    }
}

+ (NSArray *)keyword {
    return _keywords;
}

+ (NSString *)emptyModuleName {
    return _emptyModuleName;
}

+ (NSString *)defaultModuleName {
    return _defaultModuleName;
}

+ (NSString *)defaultModuleDescription {
    return _defaultModuleDescription;
}

+ (NSString *)coreModuleName {
    return _coreModuleName;
}

+ (NSString *)exports {
    return _exports;
}

+ (NSString *)imports {
    return _imports;
}

+ (NSString *)internal {
    return _internal;
}

+ (NSString *)name {
    return _name;
}

+ (NSString *)description {
    return _description;
}

+ (NSString *)ascendingKeyword {
    return _ascendingKeyword;
}

+ (NSString *)descendingKeyword {
    return _descendingKeyword;
}

+ (NSString *)keyKeyword {
    return _keyKeyword;
}

+ (NSString *)valueKeyword {
    return _valueKeyword;
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
