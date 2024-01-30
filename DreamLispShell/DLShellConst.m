//
//  DLShellConst.m
//  DreamLispShell
//
//  Created by jsloop on 25/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
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
    return [NSString stringWithFormat:@"Shell v%@ (%@)\n", version, build];
}

@end
