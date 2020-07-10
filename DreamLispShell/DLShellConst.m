//
//  DLShellConst.m
//  DreamLispShell
//
//  Created by Jaseem V V on 25/06/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLShellConst.h"

@implementation DLShellConst {
    NSString *_shellVersion;
}

+ (NSDictionary *)infoDictionary {
    return [[NSBundle bundleForClass:[self class]] infoDictionary];
}

+ (NSString *)shellVersion {
    NSDictionary *info = [self infoDictionary];
    NSString *version = [info valueForKey:@"CFBundleShortVersionString"];
    NSString *build = [info valueForKey:@"CFBundleVersion"];
    return [[NSString alloc] initWithFormat:@"Shell v%@ (%@)\n", version, build];
}

@end
