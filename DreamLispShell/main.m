//
//  main.m
//  DreamLispShell
//
//  Created by Jaseem V V on 05/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTerminal.h"
#import "DLShellConst.h"
#import "DLShellInput.h"

/** The main entry point. */
int main(int argc, const char * argv[]) {
    DreamLisp *dl = [[DreamLisp alloc] init];
    DLTerminal *term = [DLTerminal new];
    if (argc > 1) {
        @try {
            [dl setIsREPL:NO];
            [dl bootstrap];
            [[dl ioService] setStdIODelegate:term];
            [dl loadDLModuleLibs];
            [dl printVersion];
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
    [dl setIsREPL:YES];
    [dl bootstrap];
    [[dl ioService] setStdIODelegate:term];
    [dl printVersion];
    [dl loadDLModuleLibs];
    [[dl ioService] writeOutput:[DLShellConst shellVersion]];
    
    DLShellInput *inp;
    NSString *prompt = [[NSString alloc] init];
    NSMutableString *expr = [[NSMutableString alloc] init];
    NSString *line = [[NSString alloc] init];
    NSString *ret;
    
    while (true) {
        @autoreleasepool {
            @try {
                prompt = [NSString stringWithFormat:@"λ %@> ", [DLState currentModuleName]];
                inp = [term readlineWithPrompt:[prompt UTF8String]];
                if ([inp shouldEvaluate]) {
                    line = [inp expr];
                    if (line != nil && [line isNotEmpty] && [line isNotEqualTo: @"\n"]) {
                        [expr appendString:[inp expr]];
                        ret = [dl rep:expr];
                        if (ret) {
                            [[dl ioService] writeOutput:ret];
                        }
                        ret = nil;
                        inp = nil;
                        [expr setString:@""];
                    }
                    line = nil;
                } else {
                    [expr appendString:[inp expr]];
                }
            } @catch (NSException *exception) {
                [dl printException:exception log:YES readably:YES];
                ret = nil;
                inp = nil;
                line = nil;
                [expr setString:@""];
            }
        }
    }
    return 0;
}
