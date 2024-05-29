//
//  DLStack.m
//  DreamLispShell
//
//  Created by Jaseem V V on 18.05.2024.
//  Copyright © 2024 Jaseem V V. All rights reserved.
//

#import "DLStack.h"

@interface DLStack ()
@property (nonatomic, strong) NSMutableArray *arr;
@end

/** Last in, first out */
@implementation DLStack

-(instancetype)init {
    self = [super init];
    _arr = [[NSMutableArray alloc] init];
    _isInStringMode = NO;
    return self;
}

- (void)push:(NSString *)elem {
    if (elem)
    [self.arr addObject:elem];
}

- (NSString *)pop {
    id last = [self.arr lastObject];
    [self.arr removeLastObject];
    return last;
}

- (NSString *)top {
    return [self.arr lastObject];
}

- (void)popTill:(NSString *)elem {
    while ([[self top] isNotEqualTo: elem]) {
        [self pop];
    }
    if ([[self top] isEqual:elem]) {
        [self pop];
    }
}

- (NSUInteger)count {
    return [self.arr count];
}

- (BOOL)isEmpty {
    return [self.arr count] == 0;
}

- (void)reset {
    [self.arr removeAllObjects];
    self.isInStringMode = NO;
}

- (NSString *)description {
    return [NSString stringWithFormat:@"Stack: %@\nisInStringMode: %hhd", self.arr, self.isInStringMode];
}

- (NSString *)debugDescription {
    return [NSString stringWithFormat:@"Stack: %@\nisInStringMode: %hhd", self.arr, self.isInStringMode];
}

@end
