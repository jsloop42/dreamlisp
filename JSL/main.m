//
//  main.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSL.h"
#import "Types.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString* str = @"A";
        JSL* jsl = [[JSL alloc] init];
        NSString* ret = [jsl rep:str];
        NSLog(@"Ret: %@", ret);

        JSHashMap* hm = [[JSHashMap alloc] init];
        [hm setValue:@"foo" forKey:@"bar"];
        NSLog(@"%@", [hm valueForKey:@"bar"]);

        JSString* str1 = [[JSString alloc] initWithString:@"Foo"];
        NSLog(@"%@", [str1 value]);
    }
    return 0;
}
