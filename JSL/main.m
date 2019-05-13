//
//  main.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSL.h"
#import "Terminal.h"
#import "Logger.h"

/** The main entry point. */
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        JSL *jsl = [JSL new];
        if (argc > 1) {
            @try {
                [jsl setIsREPL:NO];
                NSString *jslFile = [[NSString alloc] initWithCString:argv[1] encoding:NSUTF8StringEncoding];
                NSMutableArray *arr = [NSMutableArray new];
                NSUInteger i = 0;
                if (argc > 2) {
                    for (i = 2; i < argc; i++) {
                        arr[i - 2] = [[JSString alloc] initWithCString:argv[i]];
                    }
                }
                [[jsl env] setObject:[[JSList alloc] initWithArray:arr] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
                [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", jslFile]];
                exit(0);
            } @catch (NSException *exception) {
                [jsl printException:exception log:YES readably:YES];
                exit(-1);
            }
        }
        [jsl rep:@"(println *version*)"];
        Terminal *term = [Terminal new];
        NSString *inp;
        NSString *ret;
        while (true) {
            @try {
                inp = [term readline];
                if (inp && [inp isNotEmpty]) {
                    ret = [jsl rep:inp];
                    if (ret) {
                        info(@"%@", ret);
                    }
                }
            } @catch (NSException *exception) {
                [jsl printException:exception log:YES readably:YES];
            }
        }
    }
    return 0;
}
