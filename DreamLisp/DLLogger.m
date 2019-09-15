//
//  DLLogger.m
//  DreamLisp
//
//  Created by jsloop on 12/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
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
    NSString *msg = [[NSString alloc] initWithFormat:@"[info] %@", message];
    [_ioService writeOutput:msg];
    [msg release];
}

+ (void)infoWithFormat:(NSString *)format, ... {
    va_list args;
    va_start(args, format);
    NSString *fmt = [[NSString alloc] initWithFormat:format arguments:args];
    NSString *msg = [[NSString alloc] initWithFormat:@"[info] %@", fmt];
    va_end(args);
    [_ioService writeOutput:msg];
    [msg release];
    [fmt release];
}

+ (void)debug:(NSString *)message {
    if (_isDebug) {
        NSString *msg = [[NSString alloc] initWithFormat:@"[debug] %@", message];
        [_ioService writeOutput:msg];
        [msg release];
    }
}

+ (void)debugWithFormat:(NSString *)format, ... {
    if (_isDebug) {
        va_list args;
        va_start(args, format);
        NSString *fmt = [[NSString alloc] initWithFormat:format arguments:args];
        NSString *msg = [[NSString alloc] initWithFormat:@"[debug] %@", fmt];
        va_end(args);
        [_ioService writeOutput:msg];
        [msg release];
        [fmt release];
    }
}

+ (void)error:(NSString *)message {
    NSString *msg = [[NSString alloc] initWithFormat:@"[error] %@", message];
    [_ioService writeOutput:msg];
    [msg release];
}

+ (void)errorWithFormat:(NSString *)format, ... {
    va_list args;
    va_start(args, format);
    NSString *fmt = [[NSString alloc] initWithFormat:format arguments:args];
    NSString *msg = [[NSString alloc] initWithFormat:@"[error] %@", fmt];
    va_end(args);
    [_ioService writeOutput:msg];
    [msg release];
    [fmt release];
}

+ (void)verbose:(NSString *)message {
    if (_isVerbose) {
        NSString *msg = [[NSString alloc] initWithFormat:@"[verbose] %@", message];
        [_ioService writeOutput:msg];
        [msg release];
    }
}

+ (void)verboseWithFormat:(NSString *)format, ... {
    if (_isVerbose) {
        va_list args;
        va_start(args, format);
        NSString *fmt = [[NSString alloc] initWithFormat:format arguments:args];
        NSString *msg = [[NSString alloc] initWithFormat:@"[verbose] %@", fmt];
        va_end(args);
        [_ioService writeOutput:msg];
        [msg release];
        [fmt release];
    }
}

@end
