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
static BOOL _startExpCapture = NO;
static NSMapTable<NSString *, JSSymbol *> *_expTable;

@implementation SymbolTable

@synthesize table = _table;
@synthesize expTable = _expTable;

+ (void)initialize {
    if (self == [self class]) {
        _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
        _expTable = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    }
}

+ (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol {
    NSString *key = (NSString *)[symbol initialValue];
    return _startExpCapture ? [_expTable objectForKey:key] : [_table objectForKey:key];
}

+ (void)setSymbol:(JSSymbol *)symbol {
    NSString *key = (NSString *)[symbol initialValue];
    _startExpCapture ? [_expTable setObject:symbol forKey:key] : [_table setObject:symbol forKey:key];
}

+ (void)removeSymbol:(JSSymbol *)symbol {
    NSString *key = (NSString *)[symbol initialValue];
    _startExpCapture ? [_expTable removeObjectForKey:key] : [_table removeObjectForKey:key];
}

/** Starts tracking symbols bounded within a lexical scope */
+ (void)startExpressionSymbolCapture {
    _startExpCapture = YES;
}

+ (void)stopExpressionSymbolCapture {
    _startExpCapture = NO;
}

+ (BOOL)isCapture {
    return _startExpCapture;
}

+ (void)clearSymbolCapture {
    [_expTable removeAllObjects];
}

+ (NSUInteger)count {
    return [_table count];
}

+ (NSUInteger)captureCount {
    return [_expTable count];
}

+ (NSString *)description {
    return [[_table description] stringByAppendingFormat:@"\n%@", [_expTable description]];
}

/** Increments the auto gensymn counter. */
+ (NSUInteger)counter {
    return ++_genSymCounter;
}

/** Return the current auto gensymn counter. */
+ (NSInteger)currentCounter {
    return _genSymCounter;
}

@end
