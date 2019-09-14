//
//  NSNumber+DLNumber.m
//  DreamLisp
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
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
