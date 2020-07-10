//
//  Const.m
//  DreamLisp
//
//  Created by Jaseem V V on 06/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLConst.h"

NSArray *_keywords;
NSArray *_objcMethodTypeAttribList;
NSString *_emptyModuleName;
NSString *_defaultModuleName;
NSString *_defaultModuleDescription;
NSString *_coreModuleName;
NSString *_networkModuleName;
NSString *_testModuleName;
NSMutableArray *_dlModuleLibs;
NSString *_exports;
NSString *_imports;
NSString *_internal;
NSString *_name;
NSString *_description;
NSString *_ascendingKeyword;
NSString *_descendingKeyword;
NSString *_keyKeyword;
NSString *_valueKeyword;
NSString *_keyForNotificationKey;
NSString *_keyForNotificationValue;
NSString *_foundationPrefix;
NSString *_dreamLispPrefix;
NSString *_appHome;
NSString *_prefixBinFilePathFrag;
NSString *_prefixPlistPathFrag;
NSString *_prefixStoreName;

@implementation DLConst

@dynamic keywords;
@dynamic objcMethodTypeAttribList;
@dynamic emptyModuleName;
@dynamic defaultModuleName;
@dynamic defaultModuleDescription;
@dynamic dlModuleLibs;
@dynamic coreModuleName;
@dynamic networkModuleName;
@dynamic testModuleName;
@dynamic exports;
@dynamic imports;
@dynamic internal;
@dynamic name;
@dynamic description;
@dynamic ascendingKeyword;
@dynamic descendingKeyword;
@dynamic keyKeyword;
@dynamic valueKeyword;
/*! The key associated with a DL notification key. */
@dynamic keyForNotificationKey;
/*! The key associated with a DL notification value (the function param). */
@dynamic keyForNotificationValue;
@dynamic foundationPrefix;
@dynamic dreamLispPrefix;
@dynamic prefixBinFilePathFrag;
@dynamic prefixPlistPathFrag;
@dynamic prefixStoreName;
/*!
 The dlisp home dir where any user configuration, history will be stored.
 */
@dynamic appHome;

+ (void)initialize {
    if (self == [self class]) {
        _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*", @"defmodule",
                      @"in-module", @"import", @"export", @"remove-module"];
        _objcMethodTypeAttribList = @[[DLKeyword keywordWithString:@"nullable"]];
        _emptyModuleName = @"*";
        _defaultModuleName = @"user";
        _defaultModuleDescription = @"The default module.";
        _coreModuleName = @"core";
        _networkModuleName = @"network";
        _testModuleName = @"test";
        _dlModuleLibs = [@[_coreModuleName, _testModuleName] mutableCopy];
        _exports = @"exports";
        _imports = @"imports";
        _internal = @"internal";
        _name = @"name";
        _description = @"description";
        _ascendingKeyword = @":asc";
        _descendingKeyword = @":desc";
        _keyKeyword = @":key";
        _valueKeyword = @":value";
        _keyForNotificationKey = @"notifKey";
        _keyForNotificationValue = @"args";
        _foundationPrefix = @"NS";
        _dreamLispPrefix = @"DL";
        _appHome = @"/.dlisp";
        _prefixBinFilePathFrag = @"/Data/Prefixes";
        _prefixPlistPathFrag = @"/Data/Prefixes.plist";
        _prefixStoreName = @"DLPrefixModel";
    }
}

+ (NSString *)dlVersion {
    NSDictionary *info = [[NSBundle bundleForClass:[self class]] infoDictionary];
    NSString *version = [info valueForKey:@"CFBundleShortVersionString"];
    NSString *build = [info valueForKey:@"CFBundleVersion"];
    return [[NSString alloc] initWithFormat:@"%@ (%@)", version, build];
}

+ (NSArray *)keyword {
    return _keywords;
}

+ (NSArray *)objcMethodTypeAttribList {
    return _objcMethodTypeAttribList;
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

+ (NSString *)testModuleName {
    return _testModuleName;
}

/*! Returns an array containing all DreamLisp module library names, which are written in dlisp itself. We use this to load them at startup from the bundle. */
+ (NSMutableArray *)dlModuleLibs {
    return _dlModuleLibs;
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

+ (NSString *)keyForNotificationKey {
    return _keyForNotificationKey;
}

+ (NSString *)keyForNotificationValue {
    return _keyForNotificationValue;
}

+ (NSString *)foundationPrefix {
    return _foundationPrefix;
}

+ (NSString *)dreamLispPrefix {
    return _dreamLispPrefix;
}

+ (NSString *)appHome {
    return _appHome;
}

+ (NSString *)prefixBinFilePathFrag {
    return _prefixBinFilePathFrag;
}

+ (NSString *)prefixPlistPathFrag {
    return _prefixPlistPathFrag;
}

+ (NSString *)prefixStoreName {
    return _prefixStoreName;
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
