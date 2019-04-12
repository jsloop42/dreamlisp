//
//  main.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSL.h"
#import "Terminal.h"
#import "Logger.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        JSL *jsl = [JSL new];
        Env *env = [jsl env];
        if (argc > 1) {
            @try {
                NSString *jslFile = [[NSString alloc] initWithCString:argv[1] encoding:NSUTF8StringEncoding];
                NSMutableArray *arr = [[NSValue valueWithPointer:argv] mutableCopy];
                [arr removeObjectsAtIndexes:[[NSIndexSet alloc] initWithIndexesInRange:NSMakeRange(0, 2)]];
                [env setValue:[[JSList alloc] initWithArray:arr] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
                [jsl rep:[[NSString alloc] initWithFormat:@"(load-file %@)", jslFile] withEnv:env];
                exit(0);
            } @catch (NSException *exception) {
                error(@"%@", exception.description);
            }
        }
        Terminal *term = [Terminal new];
        NSString *inp = @"";
        NSString *ret = @"";
        // TODO: print host lang.
        while ((inp = [term readline]) != nil) {
            @try {
                ret = [jsl rep:inp withEnv:env];
                error(@"%@", ret);
            } @catch (NSException *exception) {
                error(@"%@", exception.description);
            }
        }
    }
    return 0;
}
