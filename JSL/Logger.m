//
//  Logger.m
//  JSL
//
//  Created by jsloop on 12/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Logger.h"

/** Holds the info callback function mainly used for unit tests. */
void (*callback)(id param, int tag, int counter, const char *s);
static id self;
/** A tag to distinguish each log which is passed to the callback */
static int _tag = 0;
/** A counter that increases with each callback. */
static int _logCallCounter = 0;

/** A callback function which when set will be invoked for an info log. */
void infoCallback(id param, int tag, void(*fn)(id param, int tag, int counter, const char *s)) {
    callback = fn;
    self = param;
    _tag = tag;
}

/** Clears the info callback and associated variables. */
void freeInfoCallback() {
    callback = NULL;
    self = NULL;
    _tag = 0;
    _logCallCounter = 0;
}

/** Log with the given terminator string. */
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

/** Info level log. */
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

/** Debug log which is enabled only if the @c DEBUG flag is set. */
void debug(NSString *format, ...) {
    #if DEBUG
        va_list args;
        va_start(args, format);
        NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
        va_end(args);
        fprintf(stderr, "%s\n", [message UTF8String]);
    #endif
}

/** Verbose log which is enabled only if the @c VERBOSE flag is set. */
void verbose(NSString *format, ...) {
    #if VERBOSE
        va_list args;
        va_start(args, format);
        NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
        va_end(args);
        fprintf(stderr, "%s\n", [message UTF8String]);
    #endif
}

/** Error level log. */
void error(NSString *format, ...) {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
    va_end(args);
    fprintf(stderr, "%s\n", [message UTF8String]);
}
