//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

static NSMapTable<NSString *, JSSymbol *> *_table;
static NSUInteger _genSymCounter = 0;

@implementation SymbolTable

+ (void)initialize {
    if (self == [self class]) {
        _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    }
}

+ (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol {
    return [_table objectForKey:(NSString *)[symbol initialValue]];
}

+ (void)setSymbol:(JSSymbol *)symbol {
    [_table setObject:symbol forKey:(NSString *)[symbol initialValue]];
}

+ (void)removeSymbol:(JSSymbol *)symbol {
    [_table removeObjectForKey:(NSString *)[symbol initialValue]];
}

+ (NSUInteger)count {
    return [_table count];
}

+ (NSString *)description {
    return [_table description];
}

+ (NSUInteger)counter {
    return ++_genSymCounter;
}

+ (NSInteger)currentCounter {
    return _genSymCounter;
}

@end
