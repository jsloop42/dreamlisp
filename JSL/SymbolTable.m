//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

/** A table used for store and lookup of hygienic symbols */
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

- (NSUInteger)count {
    return [_table count];
}

- (NSString *)description {
    return [_table description];
}

@end
