//
//  DLMockStdIOService.m
//  DreamLispTests
//
//  Created by Jaseem V V on 29/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLMockStdIOService.h"
#import <Foundation/Foundation.h>

@implementation DLMockStdIOService {
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

