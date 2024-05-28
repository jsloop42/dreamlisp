//
//  DLLogger.m
//  DreamLisp
//
//  Created by Jaseem V V on 12/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLLogger.h"

static BOOL _isDebug = NO;
static BOOL _isVerbose = NO;
static DLIOService *_ioService;

@implementation DLLogger

+ (BOOL)isDebug {
    return _isDebug;
}

+ (BOOL)isVerbose {
    return _isVerbose;
}

+ (void)setIsDebug:(BOOL)flag {
    _isDebug = flag;
}

+ (void)setIsVerbose:(BOOL)flag {
    _isVerbose = flag;
}

+ (void)setIOService:(DLIOService *)ioService {
    _ioService = ioService;
}

+ (void)info:(NSString *)message {
    [_ioService writeOutput:[NSString stringWithFormat:@"[info] %@", message]];
}

+ (void)infoWithFormat:(NSString *)format, ... {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat:format arguments:args];
    va_end(args);
    [_ioService writeOutput:[NSString stringWithFormat:@"[info] %@", message]];
    // [message release];
}

+ (void)debug:(NSString *)message {
    if (_isDebug) [_ioService writeOutput:[NSString stringWithFormat:@"[debug] %@", message]];
}

+ (void)debugWithFormat:(NSString *)format, ... {
    if (_isDebug) {
        va_list args;
        va_start(args, format);
        NSString *message = [[NSString alloc] initWithFormat:format arguments:args];
        va_end(args);
        [_ioService writeOutput:[NSString stringWithFormat:@"[debug] %@", message]];
        // [message release];
    }
}

+ (void)error:(NSString *)message {
    [_ioService writeOutput:[NSString stringWithFormat:@"[error] %@", message]];
}

+ (void)errorWithFormat:(NSString *)format, ... {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat:format arguments:args];
    va_end(args);
    [_ioService writeOutput:[NSString stringWithFormat:@"[error] %@", message]];
    // [message release];
}

+ (void)verbose:(NSString *)message {
    if (_isVerbose) [_ioService writeOutput:[NSString stringWithFormat:@"[verbose] %@", message]];
}

+ (void)verboseWithFormat:(NSString *)format, ... {
    if (_isVerbose) {
        va_list args;
        va_start(args, format);
        NSString *message = [[NSString alloc] initWithFormat:format arguments:args];
        va_end(args);
        [_ioService writeOutput:[NSString stringWithFormat:@"[verbose] %@", message]];
        // [message release];
    }
}

@end
