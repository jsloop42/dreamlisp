//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

/**
 A temporary table used for storing and lookup of hygienic symbols for an REP loop. The table will contain symbols encountered which are defined using def!,
 defmacro!. Since processing of macros does not happen at auto gensym stage, any symbols defined using macro functions other than defmacro! will not be added.
 The table can be cleared after each REP loop.
 */
@implementation SymbolTable {
    SymbolTable *_outer;
    NSMapTable<NSString *, JSSymbol *> *_table;
}

@synthesize outer = _outer;
@synthesize table = _table;

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (instancetype)initWithTable:(SymbolTable *)table {
    self = [super init];
    if (self) {
        [self bootstrap];
        _outer = table;
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (JSSymbol * _Nullable)symbolForKey:(NSString *)name inTable:(SymbolTable *)symTable {
    if (!symTable) return nil;
    JSSymbol *sym = [[symTable table] objectForKey:name];
    return sym ? sym : [self symbolForKey:name inTable:[symTable outer]];
}

- (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol {
    return [self symbolForKey:[symbol initialValue] inTable:self];
}

- (void)setSymbol:(JSSymbol *)symbol {
    NSString *key = (NSString *)[symbol initialValue];
    [_table setObject:symbol forKey:key];
}

- (void)clearAll {
    [_table removeAllObjects];
    if (_outer) [_outer clearAll];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSString *)description {
    return [_table description];
}

@end
