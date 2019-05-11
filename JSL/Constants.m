//
//  Constants.m
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Constants.h"

NSString * const JSLVersion = @"3.0";

#pragma mark Error constant

NSString * const JSL_ERROR_TYPE = @"Type error";
NSString * const JSL_PARENS_MISMATCH = @"Parenthesis mismatch";
NSString * const JSL_QUOTE_MARK_MISMATCH = @"Quote mark mismatch";
NSString * const JSL_TOKEN_EMPTY = @"Empty token";
NSString * const JSL_SYMBOL_NOT_FOUND = @"Symbol not found";
NSString * const JSL_INVALID_ARGUMENT = @"Invalid argument";
NSString * const JSL_INDEX_OUT_OF_BOUNDS = @"Index out of bounds";
NSString * const JSL_FILE_READ_ERROR = @"File read error";
NSString * const JSL_NOT_A_SEQUENCE_ERROR = @"seq called on non-sequence";

#pragma mark Error message

NSString * const JSL_ERROR_TYPE_MSG = @"Error type";
NSString * const JSL_PARENS_MISMATCH_MSG = @"Parenthesis unbalanced";
NSString * const JSL_QUOTE_MARK_MISMATCH_MSG = @"Quotation mark unbalanced";
NSString * const JSL_TOKEN_EMPTY_MSG = @"Token is empty";
NSString * const JSL_SYMBOL_NOT_FOUND_MSG = @"Symbol not found";
NSString * const JSL_INVALID_ARGUMENT_MSG = @"Invalid argument";
NSString * const JSL_INDEX_OUT_OF_BOUNDS_MSG = @"Index out of bounds";
NSString * const JSL_FILE_READ_ERROR_MSG = @"File read error";
NSString * const JSL_NOT_A_SEQUENCE_ERROR_MSG = @"seq called on non-sequence";
