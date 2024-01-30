//
//  DLError.h
//  DreamLisp
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

extern NSString *DLArityError;
extern NSString *DLArityAnyError;
extern NSString *DLArityGreaterThanError;
extern NSString *DLArityGreaterThanOrEqualError;
extern NSString *DLArityLessThanError;
extern NSString *DLArityLessThanOrEqualError;
extern NSString *DLArityMaxError;
extern NSString *DLArityMinError;
extern NSString *DLArityMultipleError;
extern NSString *DLArityOddError;
extern NSString *DLCollectionCountError;
extern NSString *DLCollectionCountWithPositionError;
extern NSString *DLDataTypeMismatch;
extern NSString *DLDataTypeMismatchWithName;
extern NSString *DLDataTypeMismatchWithArity;
extern NSString *DLDataTypeMismatchWithNameArity;
extern NSString *DLException;
extern NSString *DLUnderlyingException;
extern NSString *DLElementCountError;
extern NSString *DLElementCountWithPositionError;
extern NSString *DLFileNotFoundError;
extern NSString *DLFunctionArityError;
extern NSString *DLIsImmutableError;
extern NSString *DLIndexOutOfBounds;
extern NSString *DLInvalidDataType;
extern NSString *DLJSONParseError;
extern NSString *DLMacroSymbolNotFound;
extern NSString *DLModuleArityDefinitionError;
extern NSString *DLModuleEmpty;
extern NSString *DLModuleNotFound;
extern NSString *DLQuotedSymbol;
extern NSString *DLSequenceError;
extern NSString *DLSymbolNotFound;
extern NSString *DLSymbolParseError;
extern NSString *DLSymbolTableTimeout;

@interface DLError : NSObject
@property (nonatomic, readwrite, retain) NSString *description;
- (instancetype)initWithDescription:(NSString *)description;
- (instancetype)initWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (instancetype)initWithData:(id<DLDataProtocol>)data;
- (instancetype)initWithUserInfo:(NSDictionary *)data;
- (NSMutableDictionary *)value;
- (void)throw;
@end

NS_ASSUME_NONNULL_END
