//
//  Terminal.m
//  JSL
//
//  Created by jsloop on 08/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Terminal.h"

char *_prompt = "user> ";
NSString *_historyFile = @"/.jsl-history";

@implementation Terminal {
    BOOL _isHistoryEnabled;
    NSString *_historyPath;
    FileOps *fops;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _isHistoryEnabled = YES;
    fops = [FileOps new];
    _historyPath = [NSHomeDirectory() stringByAppendingString:_historyFile];
    [self loadHistoryFile:_historyPath];
}

- (void)loadHistoryFile:(NSString *)path {
    @try {
        [fops openFile:path];
        while([fops hashNext]) {
            NSString *line = [fops readLine];
            if ([line length] > 0) {
                add_history([line cStringUsingEncoding:NSUTF8StringEncoding]);
            }
        }
    } @catch (NSException *exception) {
        error(@"%@", exception.description);
    }
}

- (void)disableHistory:(BOOL)flag {
    _isHistoryEnabled = flag;
}

- (NSString *)readline {
    char *input = readline(_prompt);
    NSString *exp = @"";
    if (input) {
        if (_isHistoryEnabled) {
            add_history(input);
        }
        exp = [[NSString alloc] initWithUTF8String:input];
        [fops append:exp completion:nil];
        free(input);
    }
    return exp;
}

@end
