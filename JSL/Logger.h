//
//  Logger.h
//  JSL
//
//  Created by jsloop on 12/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern void info3(NSString *terminator, NSString *format, ...) __attribute__((format(__NSString__, 2, 3)));
extern void info(NSString *format, ...) __attribute__((format(__NSString__, 1, 2)));
extern void debug(NSString *format, ...) __attribute__((format(__NSString__, 1, 2)));
extern void verbose(NSString *format, ...) __attribute__((format(__NSString__, 1, 2)));
extern void error(NSString *format, ...) __attribute__((format(__NSString__, 1, 2)));

extern void infoCallback(id param, int tag, void(*fn)(id param, int tag, int counter, const char *s));
extern void freeInfoCallback(void);

NS_ASSUME_NONNULL_END
