//
//  main.m
//  JSLShell
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Terminal.h"
#import "ShellConst.h"

/** The main entry point. */
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        JSL *jsl = [[JSL alloc] init];
        if (argc > 1) {
            @try {
                [jsl setIsREPL:NO];
                [jsl bootstrap];
                [jsl loadCoreLib];
                NSString *jslFile = [[NSString alloc] initWithCString:argv[1] encoding:NSUTF8StringEncoding];
                NSMutableArray *arr = [NSMutableArray new];
                NSUInteger i = 0;
                if (argc > 2) {
                    for (i = 2; i < argc; i++) {
                        arr[i - 2] = [[JSString alloc] initWithCString:argv[i]];
                    }
                }
                // If there are additional arguments passed to the program, bind it to *ARGV* symbol as a list.
                [[jsl env] setObject:[[JSList alloc] initWithArray:arr] forKey:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
                [jsl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", jslFile]];
                exit(0);
            } @catch (NSException *exception) {
                [jsl printException:exception log:YES readably:YES];
                exit(-1);
            }
        }
        Terminal *term = [Terminal new];
        [jsl setIsREPL:YES];
        [jsl bootstrap];
        [[jsl ioService] setStdIODelegate:term];
        [jsl printVersion];
        [jsl loadCoreLib];
        [[jsl ioService] writeOutput:[ShellConst shellVersion]];
        NSString *inp;
        NSString *ret;
        while (true) {
            @try {
                inp = [term readlineWithPrompt:[[jsl prompt] UTF8String]];
                if (inp && [inp isNotEmpty]) {
                    ret = [jsl rep:inp];
                    if (ret) {
                        [[jsl ioService] writeOutput:ret];
                    }
                }
            } @catch (NSException *exception) {
                [jsl printException:exception log:YES readably:YES];
            }
        }
    }
    return 0;
}
