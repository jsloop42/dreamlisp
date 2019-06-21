//
//  JSError.m
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSError.h"

NSString *ArityError = @"Expected arity of %ld, but obtained %ld";
NSString *ArityAnyError = @"Expected any arity of %@, but obtained %ld";
NSString *ArityGreaterThanError = @"Expected arity to be greater than %ld, but obtained %ld";
NSString *ArityGreaterThanOrEqualError = @"Expected arity to be greater than or equal to %ld, but obtained %ld";
NSString *ArityLessThanError = @"Expected arity to be less than %ld, but obtained %ld";
NSString *ArityLessThanOrEqualError = @"Expected arity to be less than or equal to %ld, but obtained %ld";
NSString *ArityMaxError = @"Expected a maximum arity of %ld, but obtained %ld";
NSString *ArityMinError = @"Expected a minimum arity of %ld, but obtained %ld";
NSString *ArityMultipleError = @"Expected arity in multiples of %ld, but obtained %ld";
NSString *ArityOddError = @"Expected arity to be odd, but obtained %ld";
NSString *CollectionCountError = @"Expected collections with equal count of %ld, but obtained %ld.";
NSString *CollectionCountWithPositionError = @"Expected collections with equal count of %ld, but obtained %ld at %ld.";
NSString *DataTypeMismatch = @"Expected %@ but obtained '%@'";
NSString *DataTypeMismatchWithName = @"'%@' requires %@ but obtained '%@'";
NSString *DataTypeMismatchWithArity = @"Expected %@ for argument %d but obtained '%@'";
NSString *DataTypeMismatchWithNameArity = @"'%@' requires %@ for argument %d but obtained '%@'";
NSString *ElementCountError = @"Expected elements with equal count of %ld, but obtained %ld.";
NSString *ElementCountWithPositionError = @"Expected elements with equal count of %ld, but obtained %ld at %ld.";
NSString *FunctionArityError = @"Expected function but obtained a symbol";
NSString *IsImmutableError = @"%@ is immutable";
NSString *IndexOutOfBounds = @"Index %ld is out of bound for length %ld";
NSString *InvalidDataType = @"Invalid datatype %@: %@";
NSString *JSLException = @"JSLException";
NSString *JSLUnderlyingException = @"JSLUnderlyingException";
NSString *MacroSymbolNotFound = @"'%@' not found, used outside quasiquote";
NSString *ModuleArityDefinitionError = @"Module arity definition error";
NSString *ModuleEmpty = @"'%@' module is empty";
NSString *ModuleNotFound = @"'%@' module not found";
NSString *QuotedSymbol = @"Expecting quoted symbol for %@";
NSString *SequenceError = @"Not a sequence";
NSString *SymbolNotFound = @"'%@' not found";
NSString *SymbolParseError = @"Symbol parse error for %@";
NSString *SymbolTableTimeout = @"Symbol table processing timed out";

@implementation JSError {
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

- (instancetype)initWithData:(id<JSDataProtocol>)data {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = JSLException;
        [_errDict setObject:data forKey:@"jsdata"];
    }
    return self;
}

/** Initialise with underlying exception from the system. */
- (instancetype)initWithUserInfo:(NSDictionary *)data {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = JSLUnderlyingException;
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
    @throw [[NSException alloc] initWithName:JSLException reason:JSLException userInfo:_errDict];
}

@end
