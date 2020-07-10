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
        while([_fops hasNext]) {
            NSString *line = [_fops readLine];
            if ([line length] > 0) add_history([line cStringUsingEncoding:NSUTF8StringEncoding]);
            [line release];
        }
    } @catch (NSException *exception) {
        [self writeOutput:exception.description];
    }
}

- (void)disableHistory:(BOOL)flag {
    _isHistoryEnabled = flag;
}

- (NSString *)readline {
    return [self readlineWithPrompt:_prompt];
}

- (NSString * _Nullable)readlineWithPrompt:(const char *)prompt {
    NSString *exp = nil;
    char *input = readline(prompt);
    if (input) {
        if (_isHistoryEnabled) add_history(input);
        exp = [NSString stringWithFormat:@"%s\n", input];
        free(input);
        [_fops append:exp];
    }
    return exp;
}

#pragma mark - StdIOServiceDelegate

- (NSString *)readInput {
    return [self readline];
}

- (NSString *)readInputWithPrompt:(NSString *)prompt {
    return [self readlineWithPrompt:[prompt UTF8String]];
}

- (void)writeOutput:(NSString *)string {
    fprintf(stdout,"%s\n", [string UTF8String]);
}

- (void)writeOutput:(NSString *)string terminator:(NSString *)terminator {
    fprintf(stdout,"%s%s", [string UTF8String], [terminator UTF8String]);
}

@end
