//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

/**
 A table used for storing and lookup of hygienic symbols for an REP loop. The table will contain symbols encountered which are defined using def, defmacro.
 Since processing of macros does not happen at auto gensym stage, any symbols defined using macro functions other than defmacro! will not be added.
 */
@implementation SymbolTable {
    SymbolTable *_outer;
    NSMapTable<SymbolTableKey *, JSSymbol *> *_table;
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

- (void)merge:(SymbolTable *)table {
    [_table merge:[table table]];
    if ([table outer]) [_outer merge:[table outer]];
}

- (SymbolTableKey *)toKey:(JSSymbol *)symbol {
    return [[SymbolTableKey alloc] initWithSymbol:symbol];
}

- (JSSymbol * _Nullable)symbolForKey:(JSSymbol *)symbol inTable:(SymbolTable *)symTable {
    if (!symTable) return nil;
    JSSymbol *sym = [[symTable table] objectForKey:[self toKey:symbol]];
    return sym ? sym : [self symbolForKey:symbol inTable:[symTable outer]];
}

- (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol {
    return [self symbolForKey:symbol inTable:self];
}

- (void)setSymbol:(JSSymbol *)symbol {
    [_table setObject:symbol forKey:[self toKey:symbol]];
}

- (void)removeAllObjects {
    [_table removeAllObjects];
    if (_outer) [_outer removeAllObjects];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSString *)description {
    return [_table description];
}

- (nonnull id)copyWithZone:(nullable NSZone *)zone {
    return [[SymbolTable alloc] initWithTable:self];
}

- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone {
    return [[SymbolTable alloc] initWithTable:self];
}

@end
