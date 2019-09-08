//
//  Const.m
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "Const.h"

NSArray *_keywords;
NSString *_emptyModuleName;
NSString *_defaultModuleName;
NSString *_defaultModuleDescription;
NSString *_coreModuleName;
NSString *_networkModuleName;
NSString *_exports;
NSString *_imports;
NSString *_internal;
NSString *_name;
NSString *_description;
NSString *_ascendingKeyword;
NSString *_descendingKeyword;
NSString *_keyKeyword;
NSString *_valueKeyword;

@implementation Const

@dynamic keywords;
@dynamic emptyModuleName;
@dynamic defaultModuleName;
@dynamic defaultModuleDescription;
@dynamic coreModuleName;
@dynamic networkModuleName;
@dynamic exports;
@dynamic imports;
@dynamic internal;
@dynamic name;
@dynamic description;
@dynamic ascendingKeyword;
@dynamic descendingKeyword;
@dynamic keyKeyword;
@dynamic valueKeyword;

+ (void)initialize {
    if (self == [self class]) {
        _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*", @"defmodule",
                      @"in-module", @"import", @"export", @"remove-module"];
        _emptyModuleName = @"*";
        _defaultModuleName = @"user";
        _defaultModuleDescription = @"The default module.";
        _coreModuleName = @"core";
        _networkModuleName = @"network";
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

+ (NSString *)dlVersion {
    NSDictionary *info = [[NSBundle bundleForClass:[self class]] infoDictionary];
    NSString *version = [info valueForKey:@"CFBundleShortVersionString"];
    NSString *build = [info valueForKey:@"CFBundleVersion"];
    return [NSString stringWithFormat:@"%@ (%@)", version, build];
}

+(NSArray *)keyword {
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

+ (NSString *)networkModuleName {
    return _networkModuleName;
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

NSString * const DL_ERROR_TYPE = @"Type error";
NSString * const DL_PARENS_MISMATCH = @"Parenthesis mismatch";
NSString * const DL_QUOTE_MARK_MISMATCH = @"Quote mark mismatch";
NSString * const DL_TOKEN_EMPTY = @"Empty token";
NSString * const DL_SYMBOL_NOT_FOUND = @"Symbol not found";
NSString * const DL_INVALID_ARGUMENT = @"Invalid argument";
NSString * const DL_INDEX_OUT_OF_BOUNDS = @"Index out of bounds";
NSString * const DL_FILE_READ_ERROR = @"File read error";

#pragma mark Error message

NSString * const DL_ERROR_TYPE_MSG = @"Error type";
NSString * const DL_PARENS_MISMATCH_MSG = @"Parenthesis unbalanced";
NSString * const DL_QUOTE_MARK_MISMATCH_MSG = @"Quotation mark unbalanced";
NSString * const DL_TOKEN_EMPTY_MSG = @"Token is empty";
NSString * const DL_SYMBOL_NOT_FOUND_MSG = @"Symbol not found";
NSString * const DL_INVALID_ARGUMENT_MSG = @"Invalid argument";
NSString * const DL_INDEX_OUT_OF_BOUNDS_MSG = @"Index out of bounds";
NSString * const DL_FILE_READ_ERROR_MSG = @"File read error";
