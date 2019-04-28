//
//  NSArray+JSDataProtocol.m
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NSArray+JSDataProtocol.h"
#import "TypeUtils.h"

@implementation NSArray (JSDataProtocol)

-(NSString *)dataType {
    return @"NSArray";
}

- (NSMutableArray *)map:(id (^)(id arg))block {
    return [TypeUtils mapOnArray:[self mutableCopy] withBlock:block];
}

- (BOOL)isEmpty {
    return [self count] == 0;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@>", NSStringFromClass([self class]), self, [self componentsJoinedByString:@", "]];
}

@end
