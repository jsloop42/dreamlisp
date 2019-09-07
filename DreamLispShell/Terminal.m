//
//  Terminal.m
//  DreamLispShell
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "Terminal.h"

static NSString *_appHome = @"/.dlisp";
static NSString *_historyFile = @"/dlisp-history";

@implementation Terminal {
    FileOps *_fops;
    BOOL _isHistoryEnabled;
    NSString *_historyPath;
    NSString *_homeDir;
    NSString *_appHomeDir;
    NSString *_prompt;
}

@synthesize prompt = _prompt;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)bootstrap {
    _fops = [FileOps new];
    _isHistoryEnabled = YES;
    _homeDir = NSHomeDirectory();
    _appHomeDir = [_homeDir stringByAppendingString:_appHome];
    _historyPath = [_appHomeDir stringByAppendingString:_historyFile];
    _prompt = [[Const defaultModuleName] stringByAppendingString:@"> "];
    [self checkPath];
    [self loadHistoryFile:_historyPath];
}

-(void)checkPath {
    if (![_fops isDirectoryExists:_appHomeDir]) {
        [_fops createDirectoryWithIntermediate:_appHomeDir];
    }
    [_fops createFileIfNotExist:_historyPath];
}

- (void)loadHistoryFile:(NSString *)path {
    @try {
        [_fops openFile:path];
        while([_fops hasNext]) {
            @autoreleasepool {
                NSString *line = [_fops readLine];
                if ([line length] > 0) add_history([line cStringUsingEncoding:NSUTF8StringEncoding]);
            }
        }
    } @catch (NSException *exception) {
        [self writeOutput:exception.description];
    }
}

- (void)disableHistory:(BOOL)flag {
    _isHistoryEnabled = flag;
}

- (NSString *)readline {
    return [self readlineWithPrompt:[_prompt UTF8String]];
}

- (NSString * _Nullable)readlineWithPrompt:(const char *)prompt {
    @autoreleasepool {
        char *input = readline(prompt);
        NSString *exp = nil;
        if (input) {
            if (_isHistoryEnabled) add_history(input);
            exp = [[NSString alloc] initWithUTF8String:input];
            [_fops append:[exp stringByAppendingString:@"\n"] completion:nil];
            free(input);
        }
        return exp;
    }
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