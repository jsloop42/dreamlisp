//
//  TypeUtils.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "TypeUtils.h"

@implementation TypeUtils

+ (NSMutableArray *)mapOnArray:(NSMutableArray *)array withBlock:(id (^)(id arg))block {
    NSMutableArray *acc = [NSMutableArray new];
    [array enumerateObjectsUsingBlock:^(id arg, NSUInteger idx, BOOL *stop) {
        [acc addObject:block(arg)];
    }];
    return acc;
}
@end
