//
//  JSData.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSData.h"

@implementation JSData

@synthesize dataType;
@synthesize value;
@synthesize meta;

- (BOOL)hasMeta {
    return NO;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [JSData new];
    return copy;
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    id copy = [JSData new];
    return copy;
}

- (BOOL)isEqual:(id)object {
    return NO;
}

- (NSUInteger)hash {
    return random();
}

@end
