//
//  JSError.h
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

extern NSString *ArityError;
extern NSString *ArityAnyError;
extern NSString *ArityGreaterThanError;
extern NSString *ArityGreaterThanOrEqualError;
extern NSString *ArityLessThanError;
extern NSString *ArityLessThanOrEqualError;
extern NSString *ArityMaxError;
extern NSString *ArityMinError;
extern NSString *ArityMultipleError;
extern NSString *ArityOddError;
extern NSString *DataTypeMismatch;
extern NSString *DataTypeMismatchWithName;
extern NSString *DataTypeMismatchWithArity;
extern NSString *DataTypeMismatchWithNameArity;
extern NSString *FunctionArityError;
extern NSString *IndexOutOfBounds;
extern NSString *InvalidDataType;
extern NSString *JSLException;
extern NSString *JSLUnderlyingException;
extern NSString *MacroSymbolNotFound;
extern NSString *ModuleEmpty;
extern NSString *ModuleNotFound;
extern NSString *QuotedSymbol;
extern NSString *SequenceError;
extern NSString *SymbolNotFound;
extern NSString *SymbolParseError;
extern NSString *SymbolTableTimeout;

@interface JSError : NSObject
@property (nonatomic, readwrite) NSString *description;
- (instancetype)initWithDescription:(NSString *)description;
- (instancetype)initWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (instancetype)initWithData:(id<JSDataProtocol>)data;
- (instancetype)initWithUserInfo:(NSDictionary *)data;
- (NSMutableDictionary *)value;
- (void)throw;
@end

NS_ASSUME_NONNULL_END
