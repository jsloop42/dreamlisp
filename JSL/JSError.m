//
//  JSError.m
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSError.h"

NSString *ArityError = @"Expected arity of %d, but obtained %d";
NSString *ArityAnyError = @"Expected any arity of %@, but obtained %d";
NSString *ArityGreaterThanError = @"Expected arity to be greater than %d, but obtained %d";
NSString *ArityGreaterThanOrEqualError = @"Expected arity to be greater than or equal to %d, but obtained %d";
NSString *ArityLessThanError = @"Expected arity to be less than %d, but obtained %d";
NSString *ArityLessThanOrEqualError = @"Expected arity to be less than or equal to %d, but obtained %d";
NSString *ArityMaxError = @"Expected a maximum arity of %d, but obtained %d";
NSString *ArityMinError = @"Expected a minimum arity of %d, but obtained %d";
NSString *ArityMultipleError = @"Expected arity in multiples of %d, but obtained %d";
NSString *ArityOddError = @"Expected arity to be odd, but obtained %d";
NSString *DataTypeMismatch = @"Expected %@ but obtained '%@'";
NSString *DataTypeMismatchWithName = @"'%@' requires %@ but obtained '%@'";
NSString *DataTypeMismatchWithArity = @"Expected %@ for argument %d but obtained '%@'";
NSString *DataTypeMismatchWithNameArity = @"'%@' requires %@ for argument %d but obtained '%@'";
NSString *FunctionArityError = @"Expected function but obtained a symbol";
NSString *IndexOutOfBounds = @"Index %d is out of bounds of %d";
NSString *InvalidDataType = @"Invalid datatype";
NSString *JSLException = @"JSLException";
NSString *JSLUnderlyingException = @"JSLUnderlyingException";
NSString *MacroSymbolNotFound = @"'%@' not found, used outside quasiquote";
NSString *SequenceError = @"Not a sequence";
NSString *SymbolNotFound = @"'%@' not found";
NSString *SymbolParseError = @"Symbol parse error for %@";

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

- (instancetype)initWithUserInfo:(NSDictionary *)data {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = JSLUnderlyingException;
        _errDict = [data mutableCopy];
    }
    return self;
}

- (instancetype)initWithArray:(NSMutableArray *)array {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = JSLException;
        [_errDict setObject:array forKey:@"array"];
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
