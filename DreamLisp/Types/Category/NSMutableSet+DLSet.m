//
//  NSMutableSet+DLSet.m
//  DreamLisp
//
//  Created by Jaseem V V on 24.07.2024.
//  Copyright © 2024 DreamLisp. All rights reserved.
//

#import "NSMutableSet+DLSet.h"

@implementation NSMutableSet (DLSet)

- (BOOL)isEmpty {
    return [self count] == 0;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[NSSet alloc] initWithSet:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[NSMutableSet alloc] initWithSet:self];
}

@end
