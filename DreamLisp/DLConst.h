//
//  Const.h
//  DreamLisp
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLKeyword.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLConst : NSObject
@property (class, retain) NSArray *keywords;
@property (class, retain) NSArray *objcMethodTypeAttribList;
@property (class, retain) NSString *emptyModuleName;
@property (class, retain) NSString *defaultModuleName;
@property (class, retain) NSString *defaultModuleDescription;
@property (class, retain) NSString *coreModuleName;
@property (class, retain) NSString *networkModuleName;
@property (class, retain) NSString *testModuleName;
@property (class, retain) NSMutableArray *dlModuleLibs;
@property (class, retain) NSString *exports;
@property (class, retain) NSString *imports;
@property (class, retain) NSString *internal;
@property (class, retain) NSString *name;
@property (class, retain) NSString *description;
@property (class, retain) NSString *ascendingKeyword;
@property (class, retain) NSString *descendingKeyword;
@property (class, retain) NSString *keyKeyword;
@property (class, retain) NSString *valueKeyword;
@property (class, retain) NSString *keyForNotificationKey;
@property (class, retain) NSString *keyForNotificationValue;
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
