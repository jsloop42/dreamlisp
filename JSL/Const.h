//
//  Const.h
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern NSString * const JSLVersion;
extern NSArray * keywords;

@interface Const : NSObject
+ (NSArray *)keyword;
+ (NSString *)defaultModuleName;
+ (NSString *)defaultModuleDescription;
+ (NSString *)coreModuleName;
+ (NSString *)exports;
+ (NSString *)imports;
+ (NSString *)internal;
+ (NSString *)name;
+ (NSString *)description;
@end

// Error constants

extern NSString * const JSL_ERROR_TYPE;
extern NSString * const JSL_PARENS_MISMATCH;
extern NSString * const JSL_QUOTE_MARK_MISMATCH;
extern NSString * const JSL_TOKEN_EMPTY;
extern NSString * const JSL_SYMBOL_NOT_FOUND;
extern NSString * const JSL_INVALID_ARGUMENT;
extern NSString * const JSL_INDEX_OUT_OF_BOUNDS;
extern NSString * const JSL_FILE_READ_ERROR;

// Error messages
extern NSString * const JSL_ERROR_TYPE_MSG;
extern NSString * const JSL_PARENS_MISMATCH_MSG;
extern NSString * const JSL_QUOTE_MARK_MISMATCH_MSG;
extern NSString * const JSL_TOKEN_EMPTY_MSG;
extern NSString * const JSL_SYMBOL_NOT_FOUND_MSG;
extern NSString * const JSL_INVALID_ARGUMENT_MSG;
extern NSString * const JSL_INDEX_OUT_OF_BOUNDS_MSG;
extern NSString * const JSL_FILE_READ_ERROR_MSG;

NS_ASSUME_NONNULL_END
