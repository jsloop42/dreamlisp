//
//  NSNumber+DLNumber.m
//  DreamLisp
//
//  Created by Jaseem V V on 13/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "NSNumber+DLCat.h"

@implementation NSNumber (DLCat)

+ (NSString *)dataType {
    return @"NSNumber";
}

- (NSString *)dataType {
    return @"NSNumber";
}

- (NSInteger)sortValue {
    return [self integerValue];
}

@end
