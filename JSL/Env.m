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
    NSMutableDictionary *_data;
}

@synthesize outer = _outer;
@synthesize data = _data;

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
            NSString *sym = [(JSSymbol *)binds[i] name];
            if ([sym isEqual:@"&"]) {
                if ([exprs count] > i) {
                    [_data setObject:[[JSList alloc] initWithArray:[exprs subarrayWithRange:NSMakeRange(i, [exprs count] - i)]] forKey:[(JSSymbol *)binds[i + 1] name]];
                } else {
                    [_data setObject:[[JSList alloc] initWithArray:@[]] forKey:[(JSSymbol *)binds[i + 1] name]];
                }
                break;
            }
            [_data setObject:exprs[i] forKey:sym];
        }
    }
    return self;
}

- (void)bootstrap {
    _data = [NSMutableDictionary new];
}

- (void)setObject:(id<JSDataProtocol>)value forSymbol:(JSSymbol *)key {
    [_data setObject:value forKey:[key name]];
}

- (Env *)findEnvForKey:(JSSymbol *)key {
    if ([_data objectForKey:[key name]] != nil) {
        return self;
    }
    return [_outer findEnvForKey:key];
}

- (id<JSDataProtocol>)objectForSymbol:(JSSymbol *)key {
    Env * env = [self findEnvForKey:key];
    if (env != nil) {
        id<JSDataProtocol>val = [[env data] objectForKey:[key name]];
        if (val != nil) {
            return val;
        }
    }
    JSError *err = [[JSError alloc] initWithFormat:SymbolNotFound, [key name]];
    @throw [[NSException alloc] initWithName:JSL_SYMBOL_NOT_FOUND reason:[err description] userInfo:[err value]];
}

@end
