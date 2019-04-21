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

@synthesize env = _env;

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
    [self setCoreFunctionsToREPL:_env];
    [self setEvalToREPL];
    [self setJSLFuns];
}

- (void)setCoreFunctionsToREPL:(Env *)env {
    NSMutableDictionary *ns = [core namespace];
    NSArray *keys = [ns allKeys];
    NSUInteger len = [keys count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        NSString *key = keys[i];
        [env setObject:[ns objectForKey:key] forSymbol:[[JSSymbol alloc] initWithName:key]];
    }
}

/**
 Sets `eval` JSL function in the REPL environment.
*/
- (void)setEvalToREPL{
    JSData * (^fn)(NSMutableArray *arg) = ^JSData *(NSMutableArray *arg) {
        return [self eval:(JSData *)arg[0] withEnv:[self env]];
    };
    [[self env] setObject:[[JSFunction alloc] initWithFn:fn] forSymbol:[[JSSymbol alloc] initWithName:@"eval"]];
    [[self env] setObject:[JSList new] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
    [[self env] setObject:[[JSString alloc] initWithString:@"Objective-C"] forSymbol:[[JSSymbol alloc] initWithName:@"*host-language*"]];
}

- (void)setJSLFuns {
    [self rep:@"(def! not (fn* (a) (if a false true)))"];
    [self rep:@"(def! load-file (fn* (x) (eval (read-string (str \"(do \" (slurp x) \")\")))))"];
    [self rep:@"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) `(if ~(first xs) ~(if (> (count xs) 1) (nth xs 1) " \
               "(throw \"odd number of forms to cond\")) (cond ~@(rest (rest xs)))))))"];
    //[self rep:@"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))"];
    [self rep:@"(def! *gensym-counter* (atom 0))"];
    [self rep:@"(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"];
    [self rep:@"(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs))" \
               "(if ~condvar ~condvar (or ~@(rest xs)))))))))"];
    [self rep:@"(def! exit (fn* () (do (prn \"Bye.\") (exit*))))"];
}

- (JSData *)read:(NSString *)string {
    return [reader readString:string];
}

- (JSData *)evalAST:(JSData *)ast withEnv:(Env *)env {
    if ([[ast dataType] isEqual:@"JSSymbol"]) {
        return [env objectForSymbol:(JSSymbol *)ast];
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

- (BOOL)isPair:(JSData *)ast {
    if (([[ast dataType] isEqual:@"JSList"] && [(JSList *)ast count] > 0) || ([[ast dataType] isEqual:@"JSVector"] && [(JSVector *)ast count] > 0)) {
        return YES;
    }
    return NO;
}

- (JSData *)quasiQuote:(JSData *)ast {
    if (![self isPair:ast]) {
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], ast] mutableCopy]];
    }
    NSMutableArray *xs = [(JSList *)ast value];
    JSData *first = [xs first];
    if ([[first dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)first name] isEqual:@"unquote"]) {
        return [xs second];
    }
    if ([self isPair:first]) {
        NSMutableArray *list = [(JSList *)first value];
        if (![list isEmpty] && [[[list first] dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)[list first] name] isEqual:@"splice-unquote"]) {
            return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"concat"], [list second],
                                                    [self quasiQuote:[[JSList alloc] initWithArray:[xs rest]]]] mutableCopy]];
        }
    }
    return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"cons"], [self quasiQuote:first],
                                            [self quasiQuote:[[JSList alloc] initWithArray:[xs rest]]]] mutableCopy]];
}

- (BOOL)isMacroCall:(JSData *)ast env:(Env *)env {
    if ([[ast dataType] isEqual:@"JSList"]) {
        NSMutableArray *xs = [(JSList *)ast value];
        JSData *first = [xs first];
        if (first && [[first dataType] isEqual:@"JSSymbol"]) {
            JSSymbol *sym = (JSSymbol *)first;
            if ([env findEnvForKey:sym]) {
                JSData *fnData = [env objectForSymbol:sym];
                if ([[fnData dataType] isEqual:@"JSFunction"]) {
                    return [(JSFunction *)fnData isMacro];
                }
            }
        }
    }
    return NO;
}

- (JSData *)macroExpand:(JSData *)ast withEnv:(Env *)env {
    while ([self isMacroCall:ast env:env]) {
        NSMutableArray *xs = [(JSList *)ast value];
        JSFunction *fn = (JSFunction *)[env objectForSymbol:(JSSymbol *)[xs first]];
        ast = [fn apply:[xs rest]];
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
            //ast = [self macroExpand:ast withEnv:env];
//            if (![[ast dataType] isEqual:@"JSList"]) {
//                return [self evalAST:ast withEnv:env];
//            }
            NSMutableArray *xs = [(JSList *)ast value];
            if ([xs isEmpty]) {
                return ast;
            }
            if ([[[xs first] dataType] isEqual:@"JSSymbol"]) {
                // special forms
                JSSymbol *sym = (JSSymbol *)[xs first];
                if ([[sym name] isEqual:@"def!"]) {
                    JSData *val = [self eval:[xs nth:2] withEnv:env];
                    [env setObject:val forSymbol:(JSSymbol *)[xs second]];
                    return val;
                } else if ([[sym name] isEqual:@"defmacro!"]) {
                    JSFunction *fn = (JSFunction *)[self eval:[xs nth:2] withEnv:env];
                    JSFunction *macro = [[JSFunction alloc] initWithMacro:fn];
                    [env setObject:(JSData *)macro forSymbol:(JSSymbol *)[xs second]];
                    return macro;
                } else if ([[sym name] isEqual:@"try*"]) {
                    @try {
                        return [self eval:[xs second] withEnv:env];
                    } @catch (NSException *exception) {
                        if ([xs count] > 2) {
                            JSList *catchxs = (JSList *)[xs nth:2];
                            if ([[[catchxs first] dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)[catchxs first] name] isNotEqualTo:@"catch*"]) {
                                @throw [[NSException alloc] initWithName:JSL_SYMBOL_NOT_FOUND reason:JSL_SYMBOL_NOT_FOUND_MSG userInfo:@{@"errMsg": [catchxs first]}];
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
                    if ([[res dataType] isEqual:@"JSNil"] || ([[res dataType] isEqual:@"JSBool"] && [(JSBool *)res value] == NO)) {
                        ast = [xs count] > 3 ? [xs nth:3] : [JSNil new];
                    } else {
                        ast = [xs nth:2];
                    }
                    continue;
                } else if ([[sym name] isEqual:@"fn*"]) {
                    JSData * (^fn)(NSMutableArray *) = ^JSData *(NSMutableArray * arg) {
                        Env *fnEnv = [[Env alloc] initWithEnv:env binds:[(JSList *)[xs second] value] exprs:arg];
                        return [self eval:[xs nth:2] withEnv:fnEnv];
                    };
                    return [[JSFunction alloc] initWithAst:[xs nth:2] params:[(JSList *)[xs second] value] env:env macro:NO meta:nil fn:fn];
                } else if ([[sym name] isEqual:@"let*"]) {
                    Env *letEnv = [[Env alloc] initWithEnv:env];
                    NSMutableArray *bindings = [[(JSData *)[xs second] dataType] isEqual:@"JSVector"] ?
                                                    [(JSVector *)[xs second] value] : [(JSList *)[xs second] value];
                    NSUInteger len = [bindings count];
                    NSUInteger i = 0;
                    for (i = 0; i < len; i += 2) {
                        [letEnv setObject:[self eval:[bindings nth: i + 1] withEnv:letEnv] forSymbol:(JSSymbol *)[bindings nth:i]];
                    }
                    ast = [xs nth:2];
                    env = letEnv;
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
            // Function
            NSMutableArray *list = [(JSList *)[self evalAST:ast withEnv:env] value];
            if ([[[list first] dataType] isNotEqualTo:@"JSFunction"]) {
                @throw [[NSException alloc] initWithName:JSL_SYMBOL_NOT_FOUND reason:JSL_SYMBOL_NOT_FOUND_MSG userInfo:nil];
            }
            JSFunction *fn = (JSFunction *)[list first];
            NSMutableArray *rest = [list rest];
            if ([fn ast] != nil) {
                ast = [fn ast];
                env = [[Env alloc] initWithEnv:[fn env] binds:[fn params] exprs:rest];
            } else {
                return [fn apply:rest];
            }
            continue;
        } else if ([[ast dataType] isEqual:@"JSHashMap"]) {
            JSHashMap *dict = (JSHashMap *)ast;
            NSArray *keys = [dict allKeys];
            NSUInteger i = 0;
            NSUInteger len = [keys count];
            JSHashMap *ret = [JSHashMap new];
            for (i = 0; i < len; i++) {
                NSString *key = keys[i];
                JSData *object = (JSData *)[self eval:(JSData *)[dict objectForKey:key] withEnv:env];
                [ret setObject:object forKey:key];
            }
            return ret;
        } else {
            return [self evalAST:ast withEnv:env];
        }
    }
}

- (NSString *)print:(JSData *)data {
    return [printer printStringFor:data readably:YES];
}

- (NSString *)rep:(NSString *)string {
    return [self print:[self eval:[self read:string] withEnv:[self env]]];
}

@end
