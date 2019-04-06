//
//  JSL.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSL.h"

@implementation JSL

-(NSString*)read:(NSString*)str {
    return str;
}

-(NSString*)eval:(NSString*)str {
    return str;
}

-(NSString*)print:(NSString*)str {
    return str;
}

-(NSString*)rep:(NSString*)str {
    return [self print:[self eval:[self read:str]]];
}

@end
