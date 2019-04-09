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
// Error constants
extern NSString * const ERROR_TYPE;
extern NSString * const PARENS_MISMATCH;
extern NSString * const QUOTE_MARK_MISMATCH;
extern NSString * const TOKEN_EMPTY;
extern NSString * const SYMBOL_NOT_FOUND;

// Error messages
extern NSString * const ERROR_TYPE_MSG;
extern NSString * const PARENS_MISMATCH_MSG;
extern NSString * const QUOTE_MARK_MISMATCH_MSG;
extern NSString * const TOKEN_EMPTY_MSG;
extern NSString * const SYMBOL_NOT_FOUND_MSG;
@end

NS_ASSUME_NONNULL_END
