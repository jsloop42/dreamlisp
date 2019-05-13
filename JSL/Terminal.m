//
//  Terminal.m
//  JSL
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Terminal.h"

const char *prompt = "repl> ";
static NSString *_appHome = @"/.jsl";
static NSString *_historyFile = @"/jsl-history";

@implementation Terminal {
    FileOps *_fops;
    BOOL _isHistoryEnabled;
    NSString *_historyPath;
    NSString *_homeDir;
    NSString *_appHomeDir;
}

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
            NSString *line = [_fops readLine];
            if ([line length] > 0) add_history([line cStringUsingEncoding:NSUTF8StringEncoding]);
        }
    } @catch (NSException *exception) {
        error(@"%@", exception.description);
    }
}

- (void)disableHistory:(BOOL)flag {
    _isHistoryEnabled = flag;
}

- (NSString *)readline {
    return [self readlineWithPrompt:prompt];
}

- (NSString * _Nullable)readlineWithPrompt:(const char *)prompt {
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

@end
