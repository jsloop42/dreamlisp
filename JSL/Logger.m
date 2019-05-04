//
//  Logger.m
//  JSL
//
//  Created by jsloop on 12/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Logger.h"

void (*callback)(id param, int tag, int counter, const char *s);
static id self;
static int _tag = 0;
static int _logCallCounter = 0;

void infoCallback(id param, int tag, void(*fn)(id param, int tag, int counter, const char *s)) {
    callback = fn;
    self = param;
    _tag = tag;
}

void freeInfoCallback() {
    callback = NULL;
    self = NULL;
    _tag = 0;
    _logCallCounter = 0;
}

void info3(NSString *terminator, NSString *format, ...) {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
    if (self != nil && callback != nil) {
        callback(self, _tag, ++_logCallCounter, [message cStringUsingEncoding:NSUTF8StringEncoding]);
    }
    va_end(args);
    fprintf(stdout,"%s%s", [message UTF8String], [terminator UTF8String]);
}

void info(NSString *format, ...) {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
    if (self != nil && callback != nil) {
        callback(self, _tag, ++_logCallCounter, [message cStringUsingEncoding:NSUTF8StringEncoding]);
    }
    va_end(args);
    fprintf(stdout,"%s\n", [message UTF8String]);
}

void debug(NSString *format, ...) {
    #if DEBUG
        va_list args;
        va_start(args, format);
        NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
        va_end(args);
        fprintf(stderr, "%s\n", [message UTF8String]);
    #endif
}

void error(NSString *format, ...) {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
    va_end(args);
    fprintf(stderr, "%s\n", [message UTF8String]);
}
