//
//  ShellConst.m
//  JSL
//
//  Created by jsloop on 25/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "ShellConst.h"

@implementation ShellConst {
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
