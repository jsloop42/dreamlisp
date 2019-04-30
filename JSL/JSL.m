//
//  JSL.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSL.h"

@implementation JSL {
    Reader *_reader;
    Printer *_printer;
    Env *_env;
    Core *_core;
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
    _reader = [Reader new];
    _printer = [Printer new];
    _env = [Env new];
    _core = [Core new];
    [self setCoreFunctionsToREPL:_env];
    [self setEvalToREPL];
    [self setJSLFuns];
}

- (void)setCoreFunctionsToREPL:(Env *)env {
    NSMutableDictionary *ns = [_core namespace];
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
    id<JSDataProtocol>(^fn)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        return [self eval:(id<JSDataProtocol>)arg[0] withEnv:[self env]];
    };
    NSString *hostLangVersion = [[NSString alloc] initWithFormat:@"%@ %.01f", @"Objective-C", (double)OBJC_API_VERSION];
    NSString *langVersion = [[NSString alloc] initWithFormat:@"JSL v%@ [%@]", JSLVersion, hostLangVersion];
    [[self env] setObject:[[JSFunction alloc] initWithFn:fn] forSymbol:[[JSSymbol alloc] initWithName:@"eval"]];
    [[self env] setObject:[JSList new] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
    [[self env] setObject:[[JSString alloc] initWithFormat:@"%@", hostLangVersion] forSymbol:[[JSSymbol alloc] initWithName:@"*host-language*"]];
    [[self env] setObject:[[JSString alloc] initWithFormat:@"%@", langVersion] forSymbol:[[JSSymbol alloc] initWithName:@"*version*"]];
    [[self env] setObject:[JSList new] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
}

- (void)setJSLFuns {
    [self rep:@"(def! not (fn* (a) (if a false true)))"];
    [self rep:@"(def! load-file (fn* (x) (eval (read-string (str \"(do \" (slurp x) \")\")))))"];
    [self rep:@"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) `(if ~(first xs) ~(if (> (count xs) 1) (nth xs 1) " \
               "(throw \"odd number of forms to cond\")) (cond ~@(rest (rest xs)))))))"];
    [self rep:@"(def! *gensym-counter* (atom 0))"];
    [self rep:@"(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"];
    [self rep:@"(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs))" \
               "(if ~condvar ~condvar (or ~@(rest xs)))))))))"];
    [self rep:@"(def! exit (fn* () (do (println \"Bye.\") (exit*))))"];
}

- (id<JSDataProtocol>)read:(NSString *)string {
    return [_reader readString:string];
}

- (id<JSDataProtocol>)evalAST:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    if ([[ast dataType] isEqual:@"JSSymbol"]) {
        return [env objectForSymbol:(JSSymbol *)ast];
    } else if ([[ast dataType] isEqual:@"JSList"]) {
        NSMutableArray *arr = [(JSList *)ast map: ^id<JSDataProtocol>(id<JSDataProtocol> xs) {
            return [self eval:xs withEnv:env];
        }];
        return [[JSList alloc] initWithArray:arr];
    } if ([[ast dataType] isEqual:@"JSVector"]) {
        NSMutableArray *arr = [(JSVector *)ast map: ^id<JSDataProtocol>(id<JSDataProtocol> xs) {
            return [self eval:xs withEnv:env];
        }];
        return [[JSVector alloc] initWithArray:arr];
    } if ([[ast dataType] isEqual:@"JSHashMap"]) {
        NSMapTable *table = [(JSHashMap *)ast value];
        NSUInteger i = 0;
        NSArray *keys = [table allKeys];
        NSUInteger len = [keys count];
        for (i = 0; i < len; i++) {
            [table setObject:[self eval:[table objectForKey:keys[i]] withEnv:env] forKey:keys[i]];
        }
        return [[JSHashMap alloc] initWithMapTable:table];
    }
    return ast;
}

- (BOOL)isPair:(id<JSDataProtocol>)ast {
    if (([[ast dataType] isEqual:@"JSList"] && [(JSList *)ast count] > 0) || ([[ast dataType] isEqual:@"JSVector"] && [(JSVector *)ast count] > 0)) {
        return YES;
    }
    return NO;
}

- (id<JSDataProtocol>)quasiQuote:(id<JSDataProtocol>)ast {
    if (![self isPair:ast]) {
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], ast] mutableCopy]];
    }
    NSMutableArray *xs = [(JSList *)ast value];
    id<JSDataProtocol> first = [xs first];
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

- (BOOL)isMacroCall:(id<JSDataProtocol>)ast env:(Env *)env {
    if ([[ast dataType] isEqual:@"JSList"]) {
        NSMutableArray *xs = [(JSList *)ast value];
        id<JSDataProtocol> first = [xs first];
        if (first && [[first dataType] isEqual:@"JSSymbol"]) {
            JSSymbol *sym = (JSSymbol *)first;
            if ([env findEnvForKey:sym]) {
                id<JSDataProtocol> fnData = [env objectForSymbol:sym];
                if ([[fnData dataType] isEqual:@"JSFunction"]) {
                    return [(JSFunction *)fnData isMacro];
                }
            }
        }
    }
    return NO;
}

- (id<JSDataProtocol>)macroExpand:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    while ([self isMacroCall:ast env:env]) {
        NSMutableArray *xs = [(JSList *)ast value];
        JSFunction *fn = (JSFunction *)[env objectForSymbol:(JSSymbol *)[xs first]];
        ast = [fn apply:[xs rest]];
    }
    return ast;
}

- (id<JSDataProtocol>)eval:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    while (true) {
        if ([[ast dataType] isEqual:@"JSVector"]) {
            NSMutableArray *xs = [(JSVector *)ast map:^id<JSDataProtocol>(id<JSDataProtocol> obj) {
                return [self eval:obj withEnv:env];
            }];
            return [[JSVector alloc] initWithArray:xs];
        } else if ([[ast dataType] isEqual:@"JSList"]) {
            ast = [self macroExpand:ast withEnv:env];
            if (![[ast dataType] isEqual:@"JSList"]) {
                return [self evalAST:ast withEnv:env];
            }
            NSMutableArray *xs = [(JSList *)ast value];
            if ([xs isEmpty]) {
                return ast;
            }
            if ([[[xs first] dataType] isEqual:@"JSSymbol"]) {
                // special forms
                JSSymbol *sym = (JSSymbol *)[xs first];
                if ([[sym name] isEqual:@"def!"]) {
                    id<JSDataProtocol> val = [self eval:[xs nth:2] withEnv:env];
                    [env setObject:val forSymbol:(JSSymbol *)[xs second]];
                    return val;
                } else if ([[sym name] isEqual:@"defmacro!"]) {
                    JSFunction *fn = (JSFunction *)[self eval:[xs nth:2] withEnv:env];
                    JSFunction *macro = [[JSFunction alloc] initWithMacro:fn];
                    [env setObject:(id<JSDataProtocol>)macro forSymbol:(JSSymbol *)[xs second]];
                    return macro;
                } else if ([[sym name] isEqual:@"try*"]) {
                    @try {
                        return [self eval:[xs second] withEnv:env];
                    } @catch (NSException *exception) {
                        if ([xs count] > 2) {
                            JSList *catchxs = (JSList *)[xs nth:2];
                            if ([[[catchxs first] dataType] isEqual:@"JSSymbol"] && [[(JSSymbol *)[catchxs first] name] isNotEqualTo:@"catch*"]) {
                                @throw [[NSException alloc] initWithName:JSL_SYMBOL_NOT_FOUND reason:JSL_SYMBOL_NOT_FOUND_MSG userInfo:@{@"jsdata": [catchxs first]}];
                            }
                            Env *catchEnv = [[Env alloc] initWithEnv:env binds:[@[(JSSymbol *)[catchxs second]] mutableCopy]
                                                                 exprs:[@[[self exceptionInfo:exception]] mutableCopy]];
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
                    id<JSDataProtocol> last = [xs last];
                    ast = last ? last : [JSNil new];
                    continue;
                } else if ([[sym name] isEqual:@"if"]) {
                    id<JSDataProtocol> res = [self eval:[xs second] withEnv:env];
                    if ([[res dataType] isEqual:@"JSNil"] || ([[res dataType] isEqual:@"JSBool"] && [(JSBool *)res value] == NO)) {
                        ast = [xs count] > 3 ? [xs nth:3] : [JSNil new];
                    } else {
                        ast = [xs nth:2];
                    }
                    continue;
                } else if ([[sym name] isEqual:@"fn*"]) {
                    id<JSDataProtocol>(^fn)(NSMutableArray *) = ^id<JSDataProtocol>(NSMutableArray * arg) {
                        Env *fnEnv = [[Env alloc] initWithEnv:env binds:[(JSList *)[xs second] value] exprs:arg];
                        return [self eval:[xs nth:2] withEnv:fnEnv];
                    };
                    return [[JSFunction alloc] initWithAst:[xs nth:2] params:[(JSList *)[xs second] value] env:env macro:NO meta:nil fn:fn];
                } else if ([[sym name] isEqual:@"let*"]) {
                    Env *letEnv = [[Env alloc] initWithEnv:env];
                    NSMutableArray *bindings = [[(id<JSDataProtocol>)[xs second] dataType] isEqual:@"JSVector"] ?
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
                id key = keys[i];
                id<JSDataProtocol> val = (id<JSDataProtocol>)[dict objectForKey:key];
                id<JSDataProtocol> object = (id<JSDataProtocol>)[self eval:val withEnv:env];
                [ret setObject:object forKey:[self eval:(id<JSDataProtocol>)key withEnv:env]];
            }
            return ret;
        } else {
            return [self evalAST:ast withEnv:env];
        }
    }
}

- (NSString *)print:(id<JSDataProtocol>)data {
    return [_printer printStringFor:data readably:YES];
}

- (NSString *)rep:(NSString *)string {
    return [self print:[self eval:[self read:string] withEnv:[self env]]];
}

- (nullable id<JSDataProtocol>)exceptionInfo:(NSException *)exception {
    NSDictionary *info = exception.userInfo;
    if (info) {
        id<JSDataProtocol> data = (id<JSDataProtocol>)[info objectForKey:@"jsdata"];
        if (data) return data;
        NSString *desc = [info valueForKey:@"description"];
        if (desc) return [[JSString alloc] initWithString:desc];
    }
    if ([exception.description isNotEmpty]) return [[JSString alloc] initWithString:exception.description];
    return nil;
}

- (NSString *)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably {
    NSString *desc = nil;
    if (exception.userInfo != nil) {
        desc = [exception.userInfo valueForKey:@"description"];
        if (desc && log) {
            error(@"%@", desc);
        } else {
            id<JSDataProtocol> data = (id<JSDataProtocol>)[exception.userInfo valueForKey:@"jsdata"];
            if (data) {
                desc = [[NSString alloc] initWithFormat:@"Error: %@", [_printer printStringFor:data readably:readably]];
                if (desc && log) {
                    error(@"%@", desc);
                }
            }
        }
    } else {
        desc = exception.description;
        if (desc && log) {
            error(@"%@", desc);
        }
    }
    return desc;
}


@end
