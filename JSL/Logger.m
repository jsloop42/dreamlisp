//
//  Logger.m
//  JSL
//
//  Created by jsloop on 12/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Logger.h"

void (*callback)(id param, const char *s);
id self;

void infoCallback(id param, void(*fn)(id param, const char *s)) {
    callback = fn;
    self = param;
}

void info(NSString *format, ...) {
    va_list args;
    va_start(args, format);
    NSString *message = [[NSString alloc] initWithFormat: format arguments: args];
    if (self != nil && callback != nil) {
        callback(self, [message cStringUsingEncoding:NSUTF8StringEncoding]);
        self = nil;
        callback = nil;
    }
    va_end(args);
    fprintf(stderr, "%s\n", [message UTF8String]);
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
