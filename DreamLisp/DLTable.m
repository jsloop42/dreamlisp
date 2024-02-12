//
//  DLTable.m
//  DreamLisp
//
//  Created by Jaseem V V on 08/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLTable.h"

@implementation DLTable {
    NSMapTable *_table;
}

@synthesize table = _table;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithTable:(NSMapTable *)table {
    self = [super init];
    if (self) {
        [self bootstrap];
        _table = table;
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSArray *)allKeys {
    return [_table allKeys];
}

- (NSArray *)allObjects {
    return [_table allObjects];
}

- (void)clear {
    [_table removeAllObjects];
}

@end
