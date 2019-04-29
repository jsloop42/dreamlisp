//
//  JSNil.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSNil.h"

@implementation JSNil {
    JSData *_meta;
}

@synthesize meta = _meta;

+ (BOOL)isNil:(id)object {
    return [[object className] isEqual:[self className]];
}

- (instancetype)init {
    self = [super init];
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"nil";
}

- (BOOL)isEqual:(JSNil *)object {
    return [object isKindOfClass:[self class]];
}

- (NSUInteger)hash {
    return 0;
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, @"nil", _meta];
}

@end
