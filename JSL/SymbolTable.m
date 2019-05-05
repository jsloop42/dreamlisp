//
//  SymbolTable.m
//  JSL
//
//  Created by jsloop on 04/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "SymbolTable.h"

@implementation SymbolTable {
    SymbolTable *_outer;
    NSMapTable<NSString *, JSSymbol *> *_table;
    BOOL _startMacroScope;
    NSMapTable<NSString *, JSSymbol *> *_expTable;
}

@synthesize outer = _outer;
@synthesize table = _table;
@synthesize expTable = _expTable;

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
        _startMacroScope = [table isMacroScope];
    }
    return self;
}

- (void)bootstrap {
    _table = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    _expTable = [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
    _startMacroScope = NO;
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
    _startMacroScope ? [_expTable setObject:symbol forKey:key] : [_table setObject:symbol forKey:key];
}

/** Starts tracking symbols bounded within a lexical scope */
- (void)startMacroScope {
    _startMacroScope = YES;
}

- (void)stopMacroScope {
    _startMacroScope = NO;
}

- (BOOL)isMacroScope {
    return _startMacroScope;
}

- (void)resetMacroScope {
    [_expTable removeAllObjects];
}

- (NSUInteger)count {
    return [_table count];
}

- (NSUInteger)macroScopeCount {
    return [_expTable count];
}

- (NSString *)description {
    return [[_table description] stringByAppendingFormat:@"\n%@", [_expTable description]];
}

@end
