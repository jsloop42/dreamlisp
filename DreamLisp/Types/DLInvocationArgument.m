//
//  DLInvocationArgument.m
//  DreamLisp
//
//  Created by jsloop on 03/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLInvocationArgument.h"

@implementation DLInvocationArgument

- (void)dealloc {
    [DLLog debug:@"DLInvocationArgument dealloc"];
}

- (instancetype)initWithCoder:(NSCoder *)coder {
    self = [super init];
    if (self) {
        _args = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLInvocationArgument_value"];
        _type = [coder decodeObjectOfClass:[self classForCoder] forKey:@"DLInvocationArgument_meta"];
    }
    return self;
}

- (void)encodeWithCoder:(NSCoder *)coder {
    [coder encodeObject:_args forKey:@"DLInvocationArgument_value"];
    [coder encodeObject:_type forKey:@"DLInvocationArgument_type"];
}

- (Class)classForCoder {
    return [self class];
}

+ (BOOL)supportsSecureCoding {
    return YES;
}

@end
