//
//  JSError.m
//  JSL
//
//  Created by jsloop on 14/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSError.h"

NSString *SymbolNotFound = @"'%@' not found";
NSString *IndexOutOfBounds = @"Index out of bounds";
NSString *JSLException = @"JSLException";

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

- (instancetype)initWithData:(JSData *)data {
    self = [super init];
    if (self) {
        [self bootstrap];
        _description = JSLException;
        [_errDict setObject:data forKey:@"jsdata"];
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

@end
