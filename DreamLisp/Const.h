//
//  Const.h
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern NSString * const DLVersion;
extern NSArray * keywords;

@interface Const : NSObject
+ (NSString *)dlVersion;
+ (NSArray *)keyword;
+ (NSString *)emptyModuleName;
+ (NSString *)defaultModuleName;
+ (NSString *)defaultModuleDescription;
+ (NSString *)coreModuleName;
+ (NSString *)exports;
+ (NSString *)imports;
+ (NSString *)internal;
+ (NSString *)name;
+ (NSString *)description;
+ (NSString *)ascendingKeyword;
+ (NSString *)descendingKeyword;
+ (NSString *)keyKeyword;
+ (NSString *)valueKeyword;
@end

// Error constants

extern NSString * const DL_ERROR_TYPE;
extern NSString * const DL_PARENS_MISMATCH;
extern NSString * const DL_QUOTE_MARK_MISMATCH;
extern NSString * const DL_TOKEN_EMPTY;
extern NSString * const DL_SYMBOL_NOT_FOUND;
extern NSString * const DL_INVALID_ARGUMENT;
extern NSString * const DL_INDEX_OUT_OF_BOUNDS;
extern NSString * const DL_FILE_READ_ERROR;

// Error messages
extern NSString * const DL_ERROR_TYPE_MSG;
extern NSString * const DL_PARENS_MISMATCH_MSG;
extern NSString * const DL_QUOTE_MARK_MISMATCH_MSG;
extern NSString * const DL_TOKEN_EMPTY_MSG;
extern NSString * const DL_SYMBOL_NOT_FOUND_MSG;
extern NSString * const DL_INVALID_ARGUMENT_MSG;
extern NSString * const DL_INDEX_OUT_OF_BOUNDS_MSG;
extern NSString * const DL_FILE_READ_ERROR_MSG;

NS_ASSUME_NONNULL_END
