//
//  JSL.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSL.h"

@implementation JSL {
    Reader *reader;
    Printer *printer;
    Env *_env;
    Core *core;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    reader = [Reader new];
    printer = [Printer new];
    _env = [Env new];
    core = [Core new];
    [self setEvalToREPL:_env];
}

/**
 Sets `eval` JSL function in the REPL environment.
*/
- (void)setEvalToREPL:(Env *)env {
    JSData * (^fn)(NSMutableArray *arg) = ^JSData *(NSMutableArray *arg) {
        return [self eval:(JSData *)arg[0] withEnv:env];
    };
    [env setValue:[[JSFunction alloc] initWithFn:fn] forSymbol:[[JSSymbol alloc] initWithName:@"eval"]];
    [env setValue:[JSList new] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
    [env setValue:[[JSString alloc] initWithString:@"Objective-C"] forSymbol:[[JSSymbol alloc] initWithName:@"*host-language*"]];
}

- (JSData *)read:(NSString *)string {
    return [reader readString:string];
}

- (JSData *)evalAST:(JSData *)ast withEnv:(Env *)env {
    if ([[ast dataType] isEqual:@"JSSymbol"]) {
        return [env valueForSymbol:(JSSymbol *)ast];
    } else if ([[ast dataType] isEqual:@"JSList"]) {
        NSMutableArray *arr = [(JSList *)ast map: ^JSData * (JSData *xs) {
            return [self eval:xs withEnv:env];
        }];
        return [[JSList alloc] initWithArray:arr];
    } if ([[ast dataType] isEqual:@"JSVector"]) {
        NSMutableArray *arr = [(JSVector *)ast map: ^JSData * (JSData *xs) {
            return [self eval:xs withEnv:env];
        }];
        return [[JSVector alloc] initWithArray:arr];
    } if ([[ast dataType] isEqual:@"JSHashMap"]) {
        NSMutableDictionary *dict = [(JSHashMap *)ast value];
        NSUInteger i = 0;
        NSArray *keys = [dict allKeys];
        NSUInteger len = [keys count];
        for (i = 0; i < len; i++) {
            [dict setObject:[self eval:[dict objectForKey:keys[i]] withEnv:env] forKey:keys[i]];
        }
        return [[JSHashMap alloc] initWithDictionary:dict];
    }
    return ast;
}

- (JSData *) quasiQuote:(JSData *)ast {
    JSList *xs = (JSList *)ast;
    if ([xs isEmpty]) {
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], ast] mutableCopy]];
    }
    JSData *first = [xs first];
    if ([[first dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)first name] isEqual:@"unquote"]) {
        return [xs second];
    }
    JSList *list = (JSList *)first;
    if (![list isEmpty] && [[[list first] dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)[list first] name] isEqual:@"splice-unquote"]) {
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"concat"], [list second], [self quasiQuote:[list rest]]] mutableCopy]];
    }
    return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"cons"], [self quasiQuote:first], [self quasiQuote:[xs rest]]] mutableCopy]];
}

- (JSData *)macroExpand:(JSData *)ast withEnv:(Env *)env {
    while ([[ast dataType] isEqual:@"JSList"]) {
        JSList *xs = (JSList *)ast;
        JSSymbol *sym = (JSSymbol *)[xs first];
        JSData *fnData = [env valueForSymbol:sym];
        if ([[fnData dataType] isEqual:@"JSFunction"]) {
            JSFunction *fn = (JSFunction *)fnData;
            if ([fn isMacro]) {
                ast = [fn apply:[(JSList *)[xs rest] value]];
            }
        }
    }
    return ast;
}

- (JSData *)eval:(JSData *)ast withEnv:(Env *)env {
    while (true) {
        if ([[ast dataType] isEqual:@"JSVector"]) {
            NSMutableArray *xs = [(JSVector *)ast map:^JSData *(JSData *obj) {
                return [self eval:obj withEnv:env];
            }];
            return [[JSVector alloc] initWithArray:xs];
        } else if ([[ast dataType] isEqual:@"JSList"]) {
            ast = [self macroExpand:ast withEnv:env];
            if (![[ast dataType] isEqual:@"JSList"]) {
                return [self evalAST:ast withEnv:env];
            }
            JSList *xs = (JSList *)ast;
            if ([xs isEmpty]) {
                return xs;
            }
            if ([[[xs first] dataType] isEqual:@"JSSymbol"]) {
                // special forms
                JSSymbol *sym = (JSSymbol *)[xs first];
                if ([[sym name] isEqual:@"def!"]) {
                    JSData *val = [self eval:[xs nth:2] withEnv:env];
                    [env setValue:val forSymbol:(JSSymbol *)[xs first]];
                    return val;
                } else if ([[sym name] isEqual:@"defmacro!"]) {
                    JSFunction *fn = (JSFunction *)[self eval:[xs nth:2] withEnv:env];
                    JSFunction *macro = [[JSFunction alloc] initWithMacro:fn];
                    [env setValue:macro forSymbol:(JSSymbol *)[xs first]];
                    return macro;
                } else if ([[sym name] isEqual:@"try*"]) {
                    @try {
                        return [self eval:[xs second] withEnv:env];
                    } @catch (NSException *exception) {
                        if ([xs count] > 2) {
                            JSList *catchxs = (JSList *)[xs nth:2];
                            if ([[[catchxs first] dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)[catchxs first] name] isNotEqualTo:@"catch*"]) {
                                @throw [[NSException alloc] initWithName:SYMBOL_NOT_FOUND reason:SYMBOL_NOT_FOUND_MSG userInfo:@{@"errMsg": [catchxs first]}];
                            }
                            Env *catchEnv = [[Env alloc] initWithEnv:env binds:[@[(JSSymbol *)[catchxs second]] mutableCopy]
                                                               exprs:[@[exception.description] mutableCopy]];
                            return [self eval:[catchxs nth:2] withEnv:catchEnv];
                         }
                        @throw exception;
                    }
                } else if ([[sym name] isEqual:@"do"]) {
                    NSInteger i = 0;
                    NSInteger len = [xs count];
                    for (i = 1; i < len - 1; i++) {
                        [self eval:[xs nth:i] withEnv:env];
                    }
                    JSData *last = [xs last];
                    ast = last ? last : [JSNil new];
                    continue;
                } else if ([[sym name] isEqual:@"if"]) {
                    JSData *res = [self eval:[xs second] withEnv:env];
                    // TODO: check if BOOl typecast is required/
                    if ([[res dataType] isEqual:@"JSNil"] || (BOOL)res == NO) {
                        ast = [xs count] > 3 ? [xs nth:3] : [JSNil new];
                    } else {
                        ast = [xs nth:2];
                    }
                    continue;
                } else if ([[sym name] isEqual:@"fn*"]) {
                    JSData * (^fn)(NSMutableArray *) = ^JSData *(NSMutableArray * arg) {
                        Env *_env = [[Env alloc] initWithEnv:env binds:[@[[xs second]] mutableCopy] exprs:arg];
                        return [self eval:[xs nth:2] withEnv:_env];
                    };
                    return [[JSFunction alloc] initWithAst:[xs nth:2] params:[@[[xs second]] mutableCopy] env:env macro:NO meta:nil fn:fn];
                } else if ([[sym name] isEqual:@"let*"]) {
                    Env *_env = [[Env alloc] initWithEnv:env];
                    JSList *bindings = (JSList *)[xs second];
                    NSUInteger len = [bindings count];
                    NSUInteger i = 0;
                    for (i = 0; i < len; i += 2) {
                        [_env setValue:[self eval:[bindings nth: i + 2] withEnv:_env] forSymbol:(JSSymbol *)[bindings nth:i]];
                    }
                    ast = [xs nth:2];
                    env = _env;
                    continue;
                } else if ([[sym name] isEqual:@"quote"]) {
                    return [xs second];
                } else if ([[sym name] isEqual:@"quasiquote"]) {
                    ast = [self quasiQuote:[xs second]];
                    continue;
                } else if ([[sym name] isEqual:@"macroexpand"]) {
                    return [self macroExpand:[xs second] withEnv:env];
                }
            }
            JSList *list = (JSList *)[self evalAST:ast withEnv:env];
            if ([[[list first] dataType] isNotEqualTo:@"JSFunction"]) {
                @throw [[NSException alloc] initWithName:SYMBOL_NOT_FOUND reason:SYMBOL_NOT_FOUND_MSG userInfo:nil];
            }
            JSFunction *fn = (JSFunction *)[list first];
            NSMutableArray *rest = [(JSList *)[xs rest] value];
            if ([fn ast] != nil) {
                ast = [fn ast];
                env = [[Env alloc] initWithEnv:[fn env] binds:[fn params] exprs:rest];
            } else {
                return [fn apply:rest];
            }
        } else if ([[ast dataType] isEqual:@"JSHashMap"]) {
            NSMutableDictionary *dict = [(JSHashMap *)ast value];
            NSArray *value = [dict allValues];
            NSUInteger i = 0;
            NSUInteger len = [value count];
            NSMutableArray *arr = [NSMutableArray new];
            for (i = 0; i < len; i++) {
                [arr addObject:[self eval:value[i] withEnv:env]];
            }
            return [[JSList alloc] initWithArray:arr];
        } else {
            return [self evalAST:ast withEnv:env];
        }
    }
}

- (NSString *)print:(JSData *)data {
    return [printer printStringFor:data readably:YES];
}

- (NSString *)rep:(NSString *)string {
    return [self print:[self eval:[self read:string] withEnv:_env]];
}

@end
