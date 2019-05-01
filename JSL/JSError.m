//
//  JSError.m
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSError.h"

NSString *ArityError = @"Arity error. Expected arity of %d, but passed %d.";
NSString *ArityAnyError = @"Arity error. Expected any arity of %@, but passed %d.";
NSString *DataTypeMismatch = @"Data type error. Expecting %@ but obtained '%@'.";
NSString *DataTypeMismatchWithArity = @"Data type error. Expecting %@ for argument %d but obtained '%@'.";
NSString *FunctionArityError = @"Arity mismatch. Expecting function but obtained a symbol.";
NSString *IndexOutOfBounds = @"Index out of bounds. Obtained index is %d but the total count is %d.";
NSString *InvalidDataType = @"Invalid datatype.";
NSString *JSLException = @"JSLException";
NSString *JSLUnderlyingException = @"JSLUnderlyingException";
NSString *OddArityError = @"Invalid argument count of %d. Expecting even number of arguments, but obtained odd.";
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
