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

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        JSL* jsl = [JSL new];
        Terminal *term = [Terminal new];
        NSString *inp = @"";
        NSString *ret = @"";
        while ((inp = [term readline]) != nil) {
            @try {
                 ret = [jsl rep:inp];
                info(@"%@", ret);
            } @catch (NSException *exception) {
                info(@"%@", exception.description);
            }
        }
    }
    return 0;
}
