//
//  Constants.h
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface Constants : NSObject

extern NSString * const JSLVersion;

// Error constants
extern NSString * const JSL_ERROR_TYPE;
extern NSString * const JSL_PARENS_MISMATCH;
extern NSString * const JSL_QUOTE_MARK_MISMATCH;
extern NSString * const JSL_TOKEN_EMPTY;
extern NSString * const JSL_SYMBOL_NOT_FOUND;
extern NSString * const JSL_INVALID_ARGUMENT;
extern NSString * const JSL_INDEX_OUT_OF_BOUNDS;
extern NSString * const JSL_FILE_READ_ERROR;
extern NSString * const JSL_NOT_A_SEQUENCE_ERROR;

// Error messages
extern NSString * const JSL_ERROR_TYPE_MSG;
extern NSString * const JSL_PARENS_MISMATCH_MSG;
extern NSString * const JSL_QUOTE_MARK_MISMATCH_MSG;
extern NSString * const JSL_TOKEN_EMPTY_MSG;
extern NSString * const JSL_SYMBOL_NOT_FOUND_MSG;
extern NSString * const JSL_INVALID_ARGUMENT_MSG;
extern NSString * const JSL_INDEX_OUT_OF_BOUNDS_MSG;
extern NSString * const JSL_FILE_READ_ERROR_MSG;
extern NSString * const JSL_NOT_A_SEQUENCE_ERROR_MSG;
@end

NS_ASSUME_NONNULL_END
