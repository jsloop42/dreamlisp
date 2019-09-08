//
//  Const.h
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface Const : NSObject
@property (class) NSArray *keywords;
@property (class) NSString *emptyModuleName;
@property (class) NSString *defaultModuleName;
@property (class) NSString *defaultModuleDescription;
@property (class) NSString *coreModuleName;
@property (class) NSString *networkModuleName;
@property (class) NSString *exports;
@property (class) NSString *imports;
@property (class) NSString *internal;
@property (class) NSString *name;
@property (class) NSString *description;
@property (class) NSString *ascendingKeyword;
@property (class) NSString *descendingKeyword;
@property (class) NSString *keyKeyword;
@property (class) NSString *valueKeyword;
+ (NSString *)dlVersion;
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
