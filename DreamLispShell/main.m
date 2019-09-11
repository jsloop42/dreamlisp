//
//  main.m
//  DreamLispShell
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTerminal.h"
#import "DLShellConst.h"

/** The main entry point. */
int main(int argc, const char * argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    DreamLisp *dl = [[DreamLisp alloc] init];
    if (argc > 1) {
        @try {
            [dl setIsREPL:NO];
            [dl bootstrap];
            [dl loadDLModuleLibs];
            NSString *dlFile = [[NSString alloc] initWithCString:argv[1] encoding:NSUTF8StringEncoding];
            NSMutableArray *arr = [NSMutableArray new];
            NSUInteger i = 0;
            if (argc > 2) {
                for (i = 2; i < argc; i++) {
                    arr[i - 2] = [[DLString alloc] initWithCString:argv[i]];
                }
            }
            /* If there are additional arguments passed to the program, bind it to *ARGV* symbol as a list. */
            [[dl env] setObject:[[DLList alloc] initWithArray:arr] forKey:[[DLSymbol alloc] initWithName:@"*ARGV*"]];
            [dl rep:[[NSString alloc] initWithFormat:@"(load-file \"%@\")", dlFile]];
            exit(0);
        } @catch (NSException *exception) {
            [dl printException:exception log:YES readably:YES];
            exit(-1);
        }
    }
    DLTerminal *term = [DLTerminal new];
    [dl setIsREPL:YES];
    [dl bootstrap];
    [[dl ioService] setStdIODelegate:term];
    [dl printVersion];
    [dl loadDLModuleLibs];
    [[dl ioService] writeOutput:[DLShellConst shellVersion]];
    NSString *inp;
    NSString *ret;
    const char *prompt = [[dl prompt] UTF8String];
    while (true) {
        @try {
            inp = [term readlineWithPrompt:prompt];
            if (inp && [inp isNotEmpty]) {
                ret = [dl rep:inp];
                [inp release];
                if (ret) {
                    [[dl ioService] writeOutput:ret];
                    [ret release];
                }
                ret = nil;
                inp = nil;
            }
        } @catch (NSException *exception) {
            [dl printException:exception log:YES readably:YES];
        }
    }
    [pool drain];
    return 0;
}
