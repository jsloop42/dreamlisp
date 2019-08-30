//
//  MockStdIOService.m
//  DreamLispTests
//
//  Created by jsloop on 29/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "MockStdIOService.h"
#import <Foundation/Foundation.h>

@implementation MockStdIOService {
    NSString *_input;
    NSString *_output;
}

@synthesize input = _input;
@synthesize output = _output;

- (void)setInput:(NSString *)string {
    _input = string;
}

- (nonnull NSString *)readInput {
    return _input;
}

- (nonnull NSString *)readInputWithPrompt:(nonnull NSString *)prompt {
    return _input;
}

- (void)writeOutput:(nonnull NSString *)string {
    _output = string;
}

- (void)writeOutput:(nonnull NSString *)string terminator:(nonnull NSString *)terminator {
    _output = string;
}

@end

