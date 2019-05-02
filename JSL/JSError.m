//
//  JSError.m
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSError.h"

NSString *ArityError = @"Arity error. Expected arity of %d, but obtained %d.";
NSString *ArityAnyError = @"Arity error. Expected any arity of %@, but obtained %d.";
NSString *ArityGreaterThanError = @"Arity error. Expected arity to be greater than %d, but obtained %d.";
NSString *ArityGreaterThanOrEqualError = @"Arity error. Expected arity to be greater than or equal to %d, but obtained %d.";
NSString *ArityLessThanError = @"Arity error. Expected arity to be less than %d, but obtained %d.";
NSString *ArityLessThanOrEqualError = @"Arity error. Expected arity to be less than or equal to %d, but obtained %d.";
NSString *ArityMaxError = @"Arity error. Expected a maximum arity of %d, but obtained %d.";
NSString *ArityMinError = @"Arity error. Expected a minimum arity of %d, but obtained %d.";
NSString *ArityMultipleError = @"Arity error. Expected arity in mutliples of %d, but obtained %d.";
NSString *ArityOddError = @"Arity error. Expected arity to be odd, but obtained %d.";
NSString *DataTypeMismatch = @"Data type error. Expecting %@ but obtained '%@'.";
NSString *DataTypeMismatchWithArity = @"Data type error. Expecting %@ for argument %d but obtained '%@'.";
NSString *FunctionArityError = @"Arity mismatch. Expecting function but obtained a symbol.";
NSString *IndexOutOfBounds = @"Index out of bounds. Obtained index is %d but the total count is %d.";
NSString *InvalidDataType = @"Invalid datatype.";
NSString *JSLException = @"JSLException";
NSString *JSLUnderlyingException = @"JSLUnderlyingException";
NSString *SymbolNotFound = @"'%@' not found";
NSString *SymbolParseError = @"Symbol parse error for %@.";

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
