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
    SymbolTable *_symTable;
    NSArray *keywords;
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
    _core = [Core new];
    _symTable = [SymbolTable new];
    _env = [[Env alloc] initWithTable:_symTable];
    keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"macroexpand", @"try*", @"catch*"];
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
        JSFunction *fn = (JSFunction *)[ns objectForKey:key];
        [env setObject:fn forSymbol:[[JSSymbol alloc] initWithArity:[fn argsCount] string:key]];
    }
}

/**
 Sets `eval` JSL function in the REPL environment.
*/
- (void)setEvalToREPL{
    JSL * __weak weakSelf = self;
    id<JSDataProtocol>(^fn)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        JSL *this = weakSelf;
        id<JSDataProtocol> ast = (id<JSDataProtocol>)arg[0];
        return [self eval:[self updateBindingsForAST:ast table:this->_symTable] withEnv:[self env]];
    };
    NSString *hostLangVersion = [[NSString alloc] initWithFormat:@"%@ %.01f", @"Objective-C", (double)OBJC_API_VERSION];
    NSString *langVersion = [[NSString alloc] initWithFormat:@"JSL v%@ [%@]", JSLVersion, hostLangVersion];
    [[self env] setObject:[[JSFunction alloc] initWithFn:fn argCount:1] forSymbol:[[JSSymbol alloc] initWithArity:1 string:@"eval"]];
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
    [self rep:@"(def! gensym (fn* () (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"];
    [self rep:@"(def! gensym (fn* (sym) (symbol (str sym (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"];
    [self rep:@"(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs))" \
               "(if ~condvar ~condvar (or ~@(rest xs)))))))))"];
    [self rep:@"(def! exit (fn* () (do (println \"Bye.\") (exit*))))"];
}

- (id<JSDataProtocol>)read:(NSString *)string {
    return [_reader readString:string];
}

- (id<JSDataProtocol>)evalAST:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    if ([JSSymbol isSymbol:ast]) {
        return [env objectForSymbol:(JSSymbol *)ast];
    } else if ([JSList isList:ast]) {
        JSList *list = (JSList *)ast;
        NSUInteger count = [list count];
        NSUInteger i = 0;
        NSUInteger j = 0;
        NSMutableArray *arr = [NSMutableArray new];
        if ([JSSymbol isSymbol:[list first]]) {
            [arr addObject:[self eval:[[JSSymbol alloc] initWithArity:count - 1 symbol:[list first]] withEnv:env]];
            j = 1;
        }
        for(i = j; j < count; j = j + 1) {
            [arr addObject:[self eval:[list nth:j] withEnv:env]];
        }
        return [[JSList alloc] initWithArray:arr];
    } if ([JSVector isVector:ast]) {
        NSMutableArray *arr = [(JSVector *)ast map: ^id<JSDataProtocol>(id<JSDataProtocol> xs) {
            return [self eval:xs withEnv:env];
        }];
        return [[JSVector alloc] initWithArray:arr];
    } if ([JSHashMap isHashMap:ast]) {
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
    return ([JSList isList:ast] && [(JSList *)ast count] > 0) || ([JSVector isVector:ast] && [(JSVector *)ast count] > 0);
}

- (id<JSDataProtocol>)quasiQuote:(id<JSDataProtocol>)ast {
    if (![self isPair:ast]) {
        //id<JSDataProtocol>arg = [JSSymbol isSymbol:ast] ? [(JSSymbol *)ast gensym] : ast;
        id<JSDataProtocol>arg = ast;
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], arg] mutableCopy]];
    }
    JSList *lst = (JSList *)ast;
    //lst = [JSSymbol updateBindingsForAST:lst symbols:nil];
    NSMutableArray *xs = [lst value];
    id<JSDataProtocol> first = [xs first];
    if ([JSSymbol isSymbol:first] && [[(JSSymbol *)first name] isEqual:@"unquote"]) return [xs second];
    if ([self isPair:first]) {
        NSMutableArray *list = [(JSList *)first value];
        if (![list isEmpty] && [JSSymbol isSymbol:[list first]] && [[(JSSymbol *)[list first] name] isEqual:@"splice-unquote"]) {
            return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"concat"], [list second],
                                                    [self quasiQuote:[[JSList alloc] initWithArray:[xs rest]]]] mutableCopy]];
        }
    }
    return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"cons"], [self quasiQuote:first],
                                            [self quasiQuote:[[JSList alloc] initWithArray:[xs rest]]]] mutableCopy]];
}

- (BOOL)isMacroCall:(id<JSDataProtocol>)ast env:(Env *)env {
    if ([JSList isList:ast]) {
        NSMutableArray *xs = [(JSList *)ast value];
        id<JSDataProtocol> first = [xs first];
        if (first && [JSSymbol isSymbol:first]) {
            JSSymbol *sym = [[JSSymbol alloc] initWithArity:[xs count] - 1 symbol:first];
            if ([env findEnvForKey:sym]) {
                id<JSDataProtocol> fnData = [env objectForSymbol:sym];
                if ([JSFunction isFunction:fnData]) return [(JSFunction *)fnData isMacro];
            }
        }
    }
    return NO;
}

- (id<JSDataProtocol>)macroExpand:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    while ([self isMacroCall:ast env:env]) {
        NSMutableArray *xs = [(JSList *)ast value];
        JSFunction *fn = (JSFunction *)[env objectForSymbol:[[JSSymbol alloc] initWithArity:[xs count] - 1 symbol:[xs first]]];
        ast = [fn apply:[xs rest]];
    }
    return ast;
}

- (id<JSDataProtocol>)eval:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    while (true) {
        if ([JSVector isVector:ast]) {
            NSMutableArray *xs = [(JSVector *)ast map:^id<JSDataProtocol>(id<JSDataProtocol> obj) {
                return [self eval:obj withEnv:env];
            }];
            return [[JSVector alloc] initWithArray:xs];
        } else if ([JSList isList:ast]) {
            ast = [self macroExpand:ast withEnv:env];
            if (![JSList isList:ast]) return [self evalAST:ast withEnv:env];
            NSMutableArray *xs = [(JSList *)ast value];
            if ([xs isEmpty]) return ast;
            if ([JSSymbol isSymbol:[xs first]]) {
                // special forms
                JSSymbol *sym = (JSSymbol *)[xs first];
                if ([[sym name] isEqual:@"def!"]) {
                    id<JSDataProtocol> val = [self eval:[xs nth:2] withEnv:env];
                    [env setObject:val forSymbol:[JSSymbol symbolWithArityCheck:[xs second] withObject:val]];
                    return val;
                } else if ([[sym name] isEqual:@"defmacro!"]) {
                    JSFunction *fn = (JSFunction *)[self eval:[xs nth:2] withEnv:env];
                    JSFunction *macro = [[JSFunction alloc] initWithMacro:fn];
                    [env setObject:macro forSymbol:[JSSymbol symbolWithArityCheck:[xs second] withObject:macro]];
                    return macro;
                } else if ([[sym name] isEqual:@"try*"]) {
                    @try {
                        return [self eval:[xs second] withEnv:env];
                    } @catch (NSException *exception) {
                        if ([xs count] > 2) {
                            JSList *catchxs = (JSList *)[xs nth:2];
                            if ([JSSymbol isSymbol:[catchxs first]] && [[(JSSymbol *)[catchxs first] name] isNotEqualTo:@"catch*"]) {
                                [[[JSError alloc] initWithData:[catchxs first]] throw];
                            }
                            //catchxs = [JSSymbol updateBindingsForAST:catchxs symbols:nil];
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
                    if ([JSNil isNil:res] || ([JSBool isBool:res] && [(JSBool *)res value] == NO)) {
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
                    //ast = [JSSymbol updateBindingsForAST:ast symbols:nil];
                    Env *letEnv = [[Env alloc] initWithEnv:env];
                    NSMutableArray *bindings = [JSVector isVector:[xs second]] ? [(JSVector *)[xs second] value] : [(JSList *)[xs second] value];
                    NSUInteger len = [bindings count];
                    NSUInteger i = 0;
                    for (i = 0; i < len; i += 2) {
                        id<JSDataProtocol> val = [self eval:[bindings nth: i + 1] withEnv:letEnv];
                        [letEnv setObject:val forSymbol:[JSSymbol symbolWithArityCheck:[bindings nth:i] withObject:val]];
                    }
                    ast = [xs nth:2];
                    env = letEnv;
                    continue;
                } else if ([[sym name] isEqual:@"quote"]) {
                    return [xs second];
                } else if ([[sym name] isEqual:@"quasiquote"]) {
                    id<JSDataProtocol> exp = [xs second];
                    //exp = [JSSymbol isSymbol:exp] ? [(JSSymbol *)exp autoGensym] : exp;
                    ast = [self quasiQuote:exp];
                    continue;
                } else if ([[sym name] isEqual:@"macroexpand"]) {
                    return [self macroExpand:[xs second] withEnv:env];
                }
            }
            // Function
            NSMutableArray *list = [(JSList *)[self evalAST:ast withEnv:env] value];
            JSFunction *fn = [JSFunction dataToFunction:[list first]];
            NSMutableArray *rest = [list rest];
            if ([fn ast]) {
                ast = [fn ast];
                env = [[Env alloc] initWithEnv:[fn env] binds:[fn params] exprs:rest];
            } else {
                return [fn apply:rest];
            }
            continue;
        } else if ([JSHashMap isHashMap:ast]) {
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

/** If the given symbol is present in the symbol table, updates the given symbol to match. */
- (JSSymbol * _Nullable)updateSymbol:(JSSymbol *)symbol table:(SymbolTable *)table {
    JSSymbol *aSym = [table symbol:symbol];
    if (aSym) {
        [symbol setValue:[aSym value]];
        return symbol;
    }
    return nil;
}

- (JSHashMap * _Nullable)updateBindingsForHashMap:(JSHashMap *)ast table:(SymbolTable *)table {
    NSArray *keys = [ast allKeys];
    NSUInteger len = [keys count];
    NSUInteger i = 0;
    id<JSDataProtocol> obj = nil;
    for (i = 0; i < len; i++) {
        //obj =
    }
    return ast;
}

- (JSVector *)updateBindingsForVector:(JSVector *)ast table:(SymbolTable *)table {
    NSMutableArray<id<JSDataProtocol>> *arr = [ast value];
    NSLock *arrLock = [NSLock new];
    [arr enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id<JSDataProtocol>  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        JSSymbol *aSym = nil;
        // Update only the symbols in the vector for which there is a match in the symbol table
        if ([JSSymbol isSymbol:obj] && ((aSym = [self updateSymbol:obj table:table]) != nil)) {
            [arrLock lock];
            [arr update:aSym atIndex:idx];
            [arrLock unlock];
        } else if ([JSVector isVector:obj]) {
            obj = [self updateBindingsForVector:obj table:table];
        } else if ([JSList isList:obj]) {
            obj = [self updateBindingsForAST:obj table:table];
        } else if ([JSHashMap isHashMap:obj]) {
            obj = [self updateBindingsForHashMap:obj table:table];
        }
    }];
    [ast setValue:arr];
    return ast;
}

- (JSList *)updateBindingsForAST:(JSList *)ast table:(SymbolTable *)table {
    if (![JSList isList:ast]) return ast;
    NSUInteger len = [ast count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        id<JSDataProtocol> elem = [ast nth:i];
        if ([JSSymbol isSymbol:elem]) {
            JSSymbol *sym = (JSSymbol *)elem;
            if ([sym position] == 0 && [sym isEqualToName:@"let*"]) {  // (let* exp)
                SymbolTable *letTable = [[SymbolTable alloc] initWithTable:table];
                NSMutableArray *bindings = [(JSList *)[ast nth:i + 1] value]; // bindings -> list or vector (let* (x 1 y 2) ..)
                NSUInteger j = 0;
                NSUInteger blen = [bindings count];
                // Check if any of the symbols are redefined
                id<JSDataProtocol> aSym = nil;
                for (j = 0; j < blen; j += 2) {
                    // NOTE: pattern matching can be done here for let destructuring
                    aSym = bindings[j];
                    if ([JSSymbol isSymbol:aSym]) {
                        JSSymbol *elem = (JSSymbol *)aSym;
                        [elem autoGensym];  // let* binding symbols are gensymed always
                        [letTable setSymbol:elem];
                    }
                    id<JSDataProtocol> exp = [self updateBindingsForAST:bindings[j + 1] table:letTable];
                    if ([JSSymbol isSymbol:exp withName:@"let*"]) {
                        SymbolTable *innerLet = [[SymbolTable alloc] initWithTable:letTable];
                        exp = [self updateBindingsForAST:exp table:innerLet];
                        letTable = innerLet; // for the new nested let scope
                    } else if ([JSList isList:exp]) {
                        exp = [self updateBindingsForAST:exp table:letTable];
                    } else if ([JSVector isVector:exp]) {
                        exp = [self updateBindingsForVector:exp table:letTable];
                    } else if ([JSHashMap isHashMap:exp]) {
                        exp = [self updateBindingsForHashMap:exp table:letTable];
                    }
                }
                table = letTable; // for the let scope
                i++;
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"def!"]) {
                i++;
                [table setSymbol:[ast nth:i]];
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"defmacro!"]) {
                i++;
                [table setSymbol:[ast nth:i]];  // add binding name to main table
                SymbolTable *macroTable = [[SymbolTable alloc] initWithTable:table];
                i++;
                [self updateBindingsForAST:[ast nth:i] table:macroTable];
                return ast;
            } else if ([sym position] == 0 && [sym isEqualToName:@"fn*"]) {
                SymbolTable *fnTable = [[SymbolTable alloc] initWithTable:table];
                i++;
                JSList* elem = [ast nth:i];  // fn arguments
                NSMutableArray *arr = [elem value];
                NSMutableArray *symArgs = [arr mutableCopy];
                NSLock *lock = [NSLock new];
                [arr enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                    id<JSDataProtocol> arg = arr[idx];
                    if ([JSSymbol isSymbol:arg]) {
                        JSSymbol *aSym = (JSSymbol *)arg;
                        if ([[aSym value] isNotEqualTo:@"&"]) {
                            arg = [aSym autoGensym];
                            [fnTable setSymbol:arg];
                        }
                    }
                    [lock lock];
                    [symArgs setObject:arg atIndexedSubscript:idx];
                    [lock unlock];
                }];
                [elem setValue:symArgs];  //update the args list with gensymed version
                [ast update:elem atIndex:i];  // TODO: remove this -> mutable ast?
                table = fnTable;
                continue;
//          } else if ([sym position] == 0 && [sym isEqualToName:@"swap!"]) {

            } else if ([sym position] == 0 && [sym isEqualToName:@"apply"]) {

            } else if ([sym position] == 0 && [keywords containsObject:sym]) {
                continue;
            } else {
                // Update symbol in the list if there is a binding found
                [self updateSymbol:[ast nth:i] table:table];
            }
        } else if ([JSList isList:elem]) {  // (fn, args)
            elem = [self updateBindingsForAST:elem table:table];
        } else if ([JSVector isVector:elem]) {
            elem = [self updateBindingsForVector:elem table:table];
        } else if ([JSHashMap isHashMap:elem]) {
            elem = [self updateBindingsForHashMap:elem table:table];
        }
    }
    return ast;
}

- (NSString *)print:(id<JSDataProtocol>)data {
    return [_printer printStringFor:data readably:YES];
}

- (NSString *)rep:(NSString *)string {
    id<JSDataProtocol> exp = [self read:string];
    exp = [self updateBindingsForAST:exp table:_symTable];
    return [self print:[self eval:exp withEnv:[self env]]];
}

- (nullable id<JSDataProtocol>)exceptionInfo:(NSException *)exception {
    NSDictionary *info = exception.userInfo;
    if (info) {
        id<JSDataProtocol> data = (id<JSDataProtocol>)[info objectForKey:@"jsdata"];
        if (data) return data;
        NSString *desc = [info valueForKey:@"description"];
        if (desc) return [[JSString alloc] initWithString:desc];
    }
    return ([exception.description isNotEmpty]) ? [[JSString alloc] initWithString:exception.description] : nil;
}

- (NSString *)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably {
    NSString *desc = nil;
    if (exception.userInfo != nil) {
        NSDictionary *info = exception.userInfo;
        desc = [info valueForKey:@"description"];
        if (desc && log) {
            error(@"%@", desc);
        } else if ([[info allKeys] containsObject:@"jsdata"]) {
            id<JSDataProtocol> data = (id<JSDataProtocol>)[info valueForKey:@"jsdata"];
            if (data) {
                desc = [[NSString alloc] initWithFormat:@"Error: %@", [_printer printStringFor:data readably:readably]];
                if (desc && log) error(@"%@", desc);
            }
        } else if ([[info allKeys] containsObject:@"NSUnderlyingError"]) {
            NSError *err = [info valueForKey:@"NSUnderlyingError"];
            error(@"%@", [err localizedDescription]);
        }
    } else {
        desc = exception.description;
        if (desc && log) error(@"%@", desc);
    }
    return desc;
}

@end
