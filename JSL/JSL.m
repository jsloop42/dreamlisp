//
//  JSL.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSL.h"

@implementation JSL

- (NSString*)read:(NSString*)string {
    return string;
}

- (NSString*)eval:(NSString*)string {
    return string;
}

- (NSString*)print:(NSString*)string {
    return string;
}

- (NSString*)rep:(NSString*)string {
    return [self print:[self eval:[self read:string]]];
}

@end
