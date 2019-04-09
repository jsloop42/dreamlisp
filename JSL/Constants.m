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

NSString * const ERROR_TYPE = @"ERROR_TYPE";
NSString * const PARENS_MISMATCH = @"PARENS_MISMATCH";
NSString * const QUOTE_MARK_MISMATCH = @"QUOTE_MARK_MISMATCH";
NSString * const TOKEN_EMPTY = @"TOKEN_EMPTY";
NSString * const SYMBOL_NOT_FOUND = @"SYMBOL_NOT_FOUND";

#pragma mark Error message

NSString * const ERROR_TYPE_MSG = @"Error type.";
NSString * const PARENS_MISMATCH_MSG = @"Parenthesis unbalanced.";
NSString * const QUOTE_MARK_MISMATCH_MSG = @"Quotation mark unbalanced.";
NSString * const TOKEN_EMPTY_MSG = @"Token is empty.";
NSString * const SYMBOL_NOT_FOUND_MSG = @"Symbol not found.";
@end
