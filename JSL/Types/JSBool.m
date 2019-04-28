//
//  JSBool.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSBool.h"

@implementation JSBool {
    BOOL _flag;
    JSData *_meta;
}

@synthesize meta = _meta;

- (instancetype)initWithBool:(BOOL)flag {
    self = [super init];
    if (self) {
        _flag = flag;
    }
    return self;
}

- (instancetype)initWithJSBool:(JSBool *)object {
    self = [super init];
    if (self) {
        _flag = [object value];
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (BOOL)isEqual:(JSBool *)boolean {
    return _flag == [boolean value];
}

- (NSUInteger)hash {
    return _flag ? 1 : 0;
}

- (BOOL)value {
    return _flag;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _flag ? @"YES" : @"NO", _meta];
}

@end
