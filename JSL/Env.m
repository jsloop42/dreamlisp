//
//  Env.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "Env.h"

@implementation Env {
    Env *outer;
    NSMutableDictionary *data;
}

@synthesize outer = outer;
@synthesize data = data;

- (instancetype)init {
    self = [super init];
    if (self) {
        data = [NSMutableDictionary new];
    }
    return self;
}

- (instancetype)initWithEnv:(Env *)env {
    self = [super init];
    if (self) {
        outer = env;
        data = [NSMutableDictionary new];
    }
    return self;
}

- (instancetype)initWithEnv:(Env *)env binds:(NSMutableArray *)binds exprs:(NSMutableArray *)exprs {
    self = [super init];
    NSUInteger len = [binds count];
    NSUInteger i = 0;
    if (self) {
        outer = env;
        for (i = 0; i < len; i++) {
            NSString *sym = [(JSSymbol *)binds[i] name];
            if ([sym isEqual:@"&"]) {
                [data setObject:[exprs subarrayWithRange:NSMakeRange(i, len)] forKey:[(JSSymbol *)binds[i + 1] name]];
                break;
            }
            [data setObject:exprs[i] forKey:sym];
        }
    }
    return self;
}

- (void)setObject:(JSData *)value forSymbol:(JSSymbol *)key {
    [data setObject:value forKey:[key name]];
}

- (Env *)findEnvForKey:(JSSymbol *)key {
    if ([data objectForKey:[key name]] != nil) {
        return self;
    }
    return [outer findEnvForKey:key];
}

- (JSData *)objectForSymbol:(JSSymbol *)key {
    Env * env = [self findEnvForKey:key];
    if (env != nil) {
        JSData *_data = [[env data] objectForKey:[key name]];
        if (_data != nil) {
            return _data;
        }
    }
    @throw [[NSException alloc] initWithName:JSL_SYMBOL_NOT_FOUND reason:JSL_SYMBOL_NOT_FOUND_MSG
                                    userInfo:[[[JSError alloc] initWithFormat:SymbolNotFound, [key name]] value]];
}

@end
