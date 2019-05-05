//
//  Env.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Env.h"

@implementation Env {
    Env *_outer;
    NSMapTable<JSSymbol *, id<JSDataProtocol>> *_table;
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

- (instancetype)initWithEnv:(Env *)env {
    self = [super init];
    if (self) {
        [self bootstrap];
        _outer = env;
    }
    return self;
}

- (JSSymbol *)setFunctionInfo:(id<JSDataProtocol>)object symbol:(JSSymbol *)symbol {
    if ([JSFunction isFunction:object]) {
        [symbol setIsFunction:YES];
        [symbol setArity:[(JSFunction *)object argsCount]];
    }
    return symbol;
}

/**
 Initializes environment with an outer environment and binds symbols with expressions.

 @param env The outer environment.
 @param binds A array of `JSSymbol` symbols.
 @param exprs A array of `id<JSDataProtocol>` expressions.
 */
- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs {
    self = [super init];
    NSUInteger len = [binds count];
    NSUInteger i = 0;
    if (self) {
        [self bootstrap];
        _outer = env;
        for (i = 0; i < len; i++) {
            JSSymbol *sym = (JSSymbol *)binds[i];
            if ([[sym name] isEqual:@"&"]) {
                if ([exprs count] > i) {
                    [_table setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:(JSSymbol *)binds[i + 1]];
                } else {
                    [_table setObject:[[JSList alloc] initWithArray:@[]] forKey:(JSSymbol *)binds[i + 1]];
                }
                break;
            }
            [_table setObject:exprs[i] forKey:[self setFunctionInfo:exprs[i] symbol:sym]];
        }
    }
    return self;
}

- (void)bootstrap {
    _table =  [NSMapTable mapTableWithKeyOptions:NSMapTableStrongMemory valueOptions:NSMapTableStrongMemory];
}

- (void)setObject:(id<JSDataProtocol>)value forSymbol:(JSSymbol *)key {
    [_table setObject:value forKey:key];
}

- (Env *)findEnvForKey:(JSSymbol *)key {
    if ([_table objectForKey:key]) {
        return self;
    } else if (![key hasNArity]) {
        return [self findEnvForKey:[key toNArity]];
    }
    return _outer ? [_outer findEnvForKey:[key resetArity]] : nil;
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key {
    return [self objectForSymbol:key isFromSymbolTable:NO];
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key isFromSymbolTable:(BOOL)isFromSymbolTable {
    Env * env = [self findEnvForKey:key];
    if (env != nil) {
        id<JSDataProtocol>val = [[env table] objectForKey:key];
        if (val != nil) return val;
    }
    // Check for n arity symbol
    if (![key hasNArity]) return [self objectForSymbol:[key toNArity]];
    if (!isFromSymbolTable) {
        JSSymbol *sym = [SymbolTable symbol:key];
        if (sym) {
            [sym copyProperties:key];
            return [self objectForSymbol:sym isFromSymbolTable:YES];
        };
    }
    [[[JSError alloc] initWithFormat:SymbolNotFound, [key string]] throw];
    return nil;
}


@end
