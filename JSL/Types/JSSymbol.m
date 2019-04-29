//
//  JSSymbol.m
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSSymbol.h"

@implementation JSSymbol {
    NSString *_name;
    JSData *_meta;
}

@synthesize meta = _meta;

+ (BOOL)isSymbol:(id)object {
    if ([object isKindOfClass:[self class]]) {
        return YES;
    }
    return NO;
}

- (instancetype)initWithName:(NSString *)name {
    self = [super init];
    if (self) {
        _name = name;
    }
    return self;
}

- (instancetype)initWithMeta:(JSData *)meta symbol:(JSSymbol *)symbol {
    self = [super init];
    if (self) {
        _name = [symbol name];
        _meta = meta;
    }
    return self;
}

- (NSString *)dataType {
    return [self className];
}

- (NSString *)dataTypeName {
    return @"symbol";
}

- (NSString *)name {
    return _name;
}

- (BOOL)isEqual:(JSSymbol *)symbol {
    return [_name isEqualToString:[symbol name]];
}

- (NSUInteger)hash {
    return [_name hash];
}

- (BOOL)hasMeta {
    return _meta != nil;
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    id copy = [[JSSymbol alloc] initWithName:_name];
    return copy;
}

- (NSString *)description {
    return [[NSString alloc] initWithFormat:@"<%@ %p - value: %@ meta: %@>", NSStringFromClass([self class]), self, _name, _meta];
}

@end
