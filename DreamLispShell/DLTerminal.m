//
//  DLTerminal.m
//  DreamLispShell
//
//  Created by Jaseem V V on 08/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLTerminal.h"

static NSString *_appHome = @"/.dlisp";
static NSString *_historyFile = @"/dlisp-history";

@implementation DLTerminal {
    DLFileOps *_fops;
    BOOL _isHistoryEnabled;
    NSString *_historyPath;
    NSString *_homeDir;
    NSString *_appHomeDir;
    DLShellInput *inp;
    const char *_prompt;
}

@synthesize prompt = _prompt;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)bootstrap {
    _fops = [DLFileOps new];
    _isHistoryEnabled = YES;
    _homeDir = NSHomeDirectory();
    _appHomeDir = [_homeDir stringByAppendingString:_appHome];
    _historyPath = [_appHomeDir stringByAppendingString:_historyFile];
    _prompt = [self promptWithModule:DLConst.defaultModuleName];
    _stack = [[DLStack alloc] init];
    inp = [[DLShellInput alloc] init];
    [self checkPath];
    [self loadHistoryFile:_historyPath];
}

- (const char *)promptWithModule:(NSString *)moduleName {
    return [[NSString stringWithFormat:@"λ %@> ", moduleName] cStringUsingEncoding:NSUTF8StringEncoding];
}

- (void)checkPath {
    if (![_fops isDirectoryExists:_appHomeDir]) {
        [_fops createDirectoryWithIntermediate:_appHomeDir];
    }
    [_fops createFileIfNotExist:_historyPath];
}

- (void)loadHistoryFile:(NSString *)path {
    @try {
        [_fops openFileForAppending:path];
        NSString *line;
        while([_fops hasNext]) {
            line = [_fops readLine];
            if ([line length] > 0) {
                add_history([line cStringUsingEncoding:NSUTF8StringEncoding]);
                line = @"";
            }
        }
    } @catch (NSException *exception) {
        [self writeOutput:exception.description];
    }
}

- (void)disableHistory:(BOOL)flag {
    _isHistoryEnabled = flag;
}

- (DLShellInput *)readline {
    return [self readlineWithPrompt:_prompt];
}

- (DLShellInput * _Nullable)readlineWithPrompt:(const char *)prompt {
    NSString *line = [[NSString alloc] init];
    char *input = readline(prompt);
    if (input) {
        [inp reset];
        if (_isHistoryEnabled) add_history(input);
        line = [NSString stringWithFormat:@"%s\n", input];
        if (_isHistoryEnabled) [_fops append:line];
        [inp setExpr:line];
        [inp setShouldEvaluate:[self shouldEvaluate:line]];
        free(input);
    }
    return inp;
}

- (BOOL)shouldEvaluate:(NSString *)line {
    BOOL shouldEval = [self.stack isEmpty];
    NSUInteger i = 0, len = [line count];
    for (i = 0; i < len; i++) {
        unichar charCode = [line characterAtIndex:i];
        /*
         (defun greet () "hello")  ->  "hello"
         (defun greet () "(hello)")  ->  "(hello)"
         (defun greet () "\"(hello)\"")  ->  "\"(hello)\""
         (defun greet () "\\(hello\\)")  ->  "\\(hello\\)"
         
         
         1. Found: ( -> push
         2. Found: )
            2.1. Check if in string mode.
            2.2. If in string mode, ignore
            2.3  Else, pop all till ( is encountered
         3. Found: "
            3.1 If not in string mode, enable string mode flag
            3.2 If in string mode and not escaped, pop " which will be on top and exit string mode
         */
        switch (charCode) {
            case 40:  /* ( */
                if (![self.stack isInStringMode]) {
                    [self.stack push:@"("];
                }
                shouldEval = NO;
                break;
            case 41:  /* ) */
                if (![self.stack isInStringMode]) {
                    [self.stack popTill:@"("];
                }
                shouldEval = [self.stack isEmpty];
                break;
            case 34:  /* " */
                if (i > 0) {
                    unichar prevElem = [line characterAtIndex:i-1];
                    if (prevElem != 92) {  // not string escape
                        [self.stack setIsInStringMode:![self.stack isInStringMode]];
                    }
                } else {
                    [self.stack setIsInStringMode:YES];
                }
                break;
        }
    }
    return shouldEval;
}

#pragma mark - StdIOServiceDelegate

- (DLShellInput *)readInput {
    return [self readline];
}

- (DLShellInput *)readInputWithPrompt:(NSString *)prompt {
    return [self readlineWithPrompt:[prompt UTF8String]];
}

- (void)writeOutput:(NSString *)string {
    fprintf(stdout,"%s\n", [string UTF8String]);
}

- (void)writeOutput:(NSString *)string terminator:(NSString *)terminator {
    fprintf(stdout,"%s%s", [string UTF8String], [terminator UTF8String]);
}

@end
