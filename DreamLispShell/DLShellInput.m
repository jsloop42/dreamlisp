//
//  DLShellInput.m
//  DreamLispShell
//
//  Created by Jaseem V V on 19.05.2024.
//  Copyright © 2024 Jaseem V V. All rights reserved.
//

#import "DLShellInput.h"

@implementation DLShellInput

-(instancetype)init{
    self = [super init];
    _shouldEvaluate = YES;
    _expr = @"";
    return self;
}

@end
