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
extern NSString *DLClassNameParseError;
extern NSString *DLClassConformanceParseError;
extern NSString *DLClassNotFoundError;
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
extern NSString *DLInitArgValueNotFoundError;
extern NSString *DLInvalidDataType;
extern NSString *DLJSONParseError;
extern NSString *DLMakeInstanceFnTypeError;
extern NSString *DLMakeInstanceParseError;
extern NSString *DLMakeInstanceNoneSpecifiedError;
extern NSString *DLMakeInstanceNoSELFoundError;
extern NSString *DLMakeInstanceSELArgCountMismatchError;
extern NSString *DLTypeValueNotSpecifiedError;
extern NSString *DLMacroSymbolNotFound;
extern NSString *DLMethodArityError;
extern NSString *DLMethodBodyNotFoundError;
extern NSString *DLMethodClassNotSpecifiedError;
extern NSString *DLMethodExpressionParseError;
extern NSString *DLMethodNameNotFoundError;
extern NSString *DLMethodParseError;
extern NSString *DLMethodResponderNotFoundError;
extern NSString *DLMethodSignatureNotFoundError;
extern NSString *DLModuleArityDefinitionError;
extern NSString *DLModuleEmpty;
extern NSString *DLModuleNotFound;
extern NSString *DLParseError;
extern NSString *DLProtocolConformanceError;
extern NSString *DLQuotedSymbol;
extern NSString *DLRTAddMethodError;
extern NSString *DLRTAllocateClassError;
extern NSString *DLRTAddIvarError;
extern NSString *DLRTAddPropertyError;
extern NSString *DLRTSlotFormatError;
extern NSString *DLRTConformProtocolError;
extern NSString *DLRTObjectInitError;
extern NSString *DLSequenceError;
extern NSString *DLSymbolMismatchError;
extern NSString *DLSymbolNotFound;
extern NSString *DLSymbolParseError;
extern NSString *DLSymbolTableTimeout;
extern NSString *DLUnrecognizedSelectorError;

@interface DLError : NSObject
@property (nonatomic, readwrite, retain) NSString *description;
@property (nonatomic, readwrite, retain) NSMutableDictionary *info;
+ (NSException *)exceptionWithDescription:(NSString *)description;
+ (NSException *)exceptionWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (instancetype)initWithDescription:(NSString *)description;
- (instancetype)initWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (instancetype)initWithData:(id<DLDataProtocol>)data;
- (instancetype)initWithUserInfo:(NSDictionary *)data;
- (void)throw;
@end

NS_ASSUME_NONNULL_END
