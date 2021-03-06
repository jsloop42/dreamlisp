//
//  DLIOService.m
//  DreamLisp
//
//  Created by Jaseem V V on 27/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLIOService.h"

@implementation DLIOService {
    id<DLFileIOServiceDelegate> __weak _fileIODelegate;
    id<DLStdIOServiceDelegate> __weak _stdIODelegate;
}

@synthesize fileIODelegate = _fileIODelegate;
@synthesize stdIODelegate = _stdIODelegate;

- (instancetype)initWithFileIODelegate:(id<DLFileIOServiceDelegate>)aFileIODelegate stdIODelegate:(id<DLStdIOServiceDelegate>)aStdIODelegate {
    self = [super init];
    if (self) {
        _fileIODelegate = aFileIODelegate;
        _stdIODelegate = aStdIODelegate;
    }
    return self;
}

#pragma mark - File IO

- (nonnull NSString *)readFile:(nonnull NSString *)path {
    return [_fileIODelegate readFile:path];
}

#pragma mark - Std IO

- (nonnull NSString *)readInput {
    return [_stdIODelegate readInput];
}

- (NSString *)readInputWithPrompt:(NSString *)prompt {
    return [_stdIODelegate readInputWithPrompt:prompt];
}

- (void)writeOutput:(nonnull NSString *)string {
    [_stdIODelegate writeOutput:string];
}

- (void)writeOutput:(nonnull NSString *)string terminator:(nonnull NSString *)terminator {
    [_stdIODelegate writeOutput:string terminator:terminator];
}

#pragma mark - Bundle

/** Return the path from where the executable is placed. */
- (NSBundle *)mainBundle {
    return [NSBundle bundleForClass:[self class]];
}

/** Returns the main bundle resource path. */
- (NSString *)resourcePath {
    return [[self mainBundle] resourcePath];
}

@end
