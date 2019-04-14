//
//  Constants.m
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Constants.h"

@implementation Constants

#pragma mark Error constant

NSString * const JSL_ERROR_TYPE = @"ERROR_TYPE";
NSString * const JSL_PARENS_MISMATCH = @"PARENS_MISMATCH";
NSString * const JSL_QUOTE_MARK_MISMATCH = @"QUOTE_MARK_MISMATCH";
NSString * const JSL_TOKEN_EMPTY = @"TOKEN_EMPTY";
NSString * const JSL_SYMBOL_NOT_FOUND = @"SYMBOL_NOT_FOUND";
NSString * const JSL_INVALID_ARGUMENT = @"Invalid argument.";
NSString * const JSL_INDEX_OUT_OF_BOUNDS = @"Index out of bounds.";

#pragma mark Error message

NSString * const JSL_ERROR_TYPE_MSG = @"Error type.";
NSString * const JSL_PARENS_MISMATCH_MSG = @"Parenthesis unbalanced.";
NSString * const JSL_QUOTE_MARK_MISMATCH_MSG = @"Quotation mark unbalanced.";
NSString * const JSL_TOKEN_EMPTY_MSG = @"Token is empty.";
NSString * const JSL_SYMBOL_NOT_FOUND_MSG = @"Symbol not found.";
NSString * const JSL_INVALID_ARGUMENT_MSG = @"Invalid argument.";
NSString * const JSL_INDEX_OUT_OF_BOUNDS_MSG = @"Index out of bounds.";
@end
