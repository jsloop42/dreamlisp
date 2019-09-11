//
//  DLError.m
//  DreamLisp
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLError.h"

NSString *DLArityError = @"Expected arity of %ld, but obtained %ld";
NSString *DLArityAnyError = @"Expected any arity of %@, but obtained %ld";
NSString *DLArityGreaterThanError = @"Expected arity to be greater than %ld, but obtained %ld";
NSString *DLArityGreaterThanOrEqualError = @"Expected arity to be greater than or equal to %ld, but obtained %ld";
NSString *DLArityLessThanError = @"Expected arity to be less than %ld, but obtained %ld";
NSString *DLArityLessThanOrEqualError = @"Expected arity to be less than or equal to %ld, but obtained %ld";
NSString *DLArityMaxError = @"Expected a maximum arity of %ld, but obtained %ld";
NSString *DLArityMinError = @"Expected a minimum arity of %ld, but obtained %ld";
NSString *DLArityMultipleError = @"Expected arity in multiples of %ld, but obtained %ld";
NSString *DLArityOddError = @"Expected arity to be even, but obtained %ld";
NSString *DLClassConformanceParseError = @"'defclass' conformance list requires 'symbol' at %ld, but obtained '%@'";
NSString *DLCollectionCountError = @"Expected collections with equal count of %ld, but obtained %ld.";
NSString *DLCollectionCountWithPositionError = @"Expected collections with equal count of %ld, but obtained %ld at %ld";
NSString *DLDataTypeMismatch = @"Expected %@ but obtained '%@'";
NSString *DLDataTypeMismatchWithName = @"'%@' requires %@ but obtained '%@'";
NSString *DLDataTypeMismatchWithArity = @"Expected %@ for argument %d but obtained '%@'";
NSString *DLDataTypeMismatchWithNameArity = @"'%@' requires %@ for argument %d but obtained '%@'";
NSString *DLException = @"DLException";
NSString *DLUnderlyingException = @"DLUnderlyingException";
NSString *DLElementCountError = @"Expected elements with equal count of %ld, but obtained %ld.";
NSString *DLElementCountWithPositionError = @"Expected elements with equal count of %ld, but obtained %ld at %ld";
NSString *DLFileNotFoundError = @"File is not found at path %@";
NSString *DLFunctionArityError = @"Expected function but obtained a symbol";
NSString *DLIsImmutableError = @"%@ is immutable";
NSString *DLIndexOutOfBounds = @"Index %ld is out of bound for length %ld";
NSString *DLInitArgValueNotFoundError = @"Expecting value for the :initarg %@, but none found";
NSString *DLInvalidDataType = @"Invalid datatype %@: %@";
NSString *DLJSONParseError = @"Error parsing JSON %@";
NSString *DLMakeInstanceFnTypeError = @"Cannot use %@ as argument. Instead use a block.";
NSString *DLMakeInstanceNoneSpecifiedError = @"'make-instance' requires a %@, but none specified";
NSString *DLTypeValueNotSpecifiedError = @"%@ requires the type value as keyword to be specified at %ld, but found %@";
NSString *DLMacroSymbolNotFound = @"'%@' not found, used outside quasiquote";
NSString *DLModuleArityDefinitionError = @"Module arity definition error";
NSString *DLModuleEmpty = @"'%@' module is empty";
NSString *DLModuleNotFound = @"'%@' module not found";
NSString *DLProtocolConformanceError = @"Invalid object %@";
NSString *DLQuotedSymbol = @"Expecting quoted symbol for %@";
NSString *DLRTAddMethodError = @"Adding method to the class failed";
NSString *DLRTAllocateClassError = @"Allocating the class failed";
NSString *DLRTAddIvarError = @"Adding instance var failed";
NSString *DLRTAddPropertyError = @"Adding property failed";
NSString *DLRTSlotFormatError = @"Invalid slot format at %@";
NSString *DLRTConformProtocolError = @"Adding protocol conformance failed";
NSString *DLRTObjectInitError = @"Error initializing object for class %@";
NSString *DLSequenceError = @"Not a sequence";
NSString *DLSymbolMismatchError = @"Expecting '%@', but found %@";
NSString *DLSymbolNotFound = @"'%@' not found";
NSString *DLSymbolParseError = @"Symbol parse error for %@";
NSString *DLSymbolTableTimeout = @"Symbol table processing timed out";

@implementation DLError {
    NSString *_description;
    NSMutableDictionary *_errDict;
}

@synthesize description = _description;

- (instancetype)initWithDescription:(NSString *)description {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = description;
        [_errDict setValue:_description forKey:@"description"];
    }
    return self;
}

- (instancetype)initWithFormat:(NSString *)format, ... {
    self = [super init];
    if (self) {
        [self bootstrap];
        va_list args, argscopy;
        va_start(args, format);
        va_copy(argscopy, args);
        va_end(args);
        _description = [[NSString alloc] initWithFormat:format arguments:argscopy];
        [_errDict setValue:_description forKey:@"description"];
    }
    return self;
}

- (instancetype)initWithData:(id<DLDataProtocol>)data {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = DLException;
        [_errDict setObject:data forKey:@"dldata"];
    }
    return self;
}

/** Initialise with underlying exception from the system. */
- (instancetype)initWithUserInfo:(NSDictionary *)data {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = DLUnderlyingException;
        _errDict = [data mutableCopy];
    }
    return self;
}

- (void)bootstrap {
    _errDict = [NSMutableDictionary new];
}

- (NSMutableDictionary *)value {
    return _errDict;
}

- (void)throw {
    @throw [[NSException alloc] initWithName:DLException reason:DLException userInfo:_errDict];
}

@end
