//
//  main.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSL.h"
#include <readline/readline.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        JSL* jsl = [JSL new];
        const char *prompt = "user> ";
        char *input = NULL;
        NSString *inp = @"";
        NSString *ret = @"";
        while ((input = readline(prompt)) != NULL) {
            add_history(input);
            inp = [[NSString alloc] initWithUTF8String:input];
            free(input);
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
