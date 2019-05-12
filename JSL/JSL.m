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
    NSArray *_keywords;
    BOOL _isQuasiquoteMode;
    NSUInteger _quasiquoteDepth;
    NSUInteger _unquoteDepth;
    FileOps *_fileOps;
    dispatch_queue_t _queue;
}

@synthesize env = _env;

- (instancetype)init {
    self = [super init];
    if (self) [self bootstrap];
    return self;
}

- (void)bootstrap {
    _reader = [Reader new];
    _printer = [Printer new];
    _core = [Core new];
    _symTable = [SymbolTable new];
    _env = [Env new];
    _fileOps = [FileOps new];
    _isQuasiquoteMode = NO;
    _quasiquoteDepth = 0;
    _queue = dispatch_queue_create("jsl-dispatch-queu", nil);
    _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*"];
    [self setCoreFunctionsToREPL:_env];
    [self setLoadFileToREPL];
    [self setEvalToREPL];
    [self loadCoreLib];
}

#pragma mark Env setup

/** Adds functions defined in @c core module to the environment. */
- (void)setCoreFunctionsToREPL:(Env *)env {
    [_env setCoreModule:[_core module]];
}

/** Add @c eval function to the environment. */
- (void)setEvalToREPL{
    JSL * __weak weakSelf = self;
    id<JSDataProtocol>(^fn)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        JSL *this = weakSelf;
        id<JSDataProtocol> ast = (id<JSDataProtocol>)arg[0];
        return [self eval:[self updateBindingsForAST:ast table:this->_symTable] withEnv:[self env]];
    };
    NSString *hostLangVersion = [[NSString alloc] initWithFormat:@"%@ %.01f", @"Objective-C", (double)OBJC_API_VERSION];
    NSString *langVersion = [[NSString alloc] initWithFormat:@"JSL v%@ [%@]", JSLVersion, hostLangVersion];
    [[self env] setObject:[[JSFunction alloc] initWithFn:fn argCount:1 name:@"eval/1"] forSymbol:[[JSSymbol alloc] initWithArity:1 string:@"eval"]];
    [[self env] setObject:[JSList new] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
    [[self env] setObject:[[JSString alloc] initWithFormat:@"%@", hostLangVersion] forSymbol:[[JSSymbol alloc] initWithName:@"*host-language*"]];
    [[self env] setObject:[[JSString alloc] initWithFormat:@"%@", langVersion] forSymbol:[[JSSymbol alloc] initWithName:@"*version*"]];
    [[self env] setObject:[JSList new] forSymbol:[[JSSymbol alloc] initWithName:@"*ARGV*"]];
}

/** Add @c load-file to the environment, which loads and evaluates the expressions contained in the file. */
- (void)setLoadFileToREPL {
    JSL * __weak weakSelf = self;
    id<JSDataProtocol>(^loadFile)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        JSL *this = weakSelf;
        NSString *path = [[JSString dataToString:arg[0] fnName:@"load-file/1"] value];
        [[this->_fileOps loadFileFromPath:[@[path] mutableCopy] isConcurrent:NO isLookup:NO]
         enumerateObjectsUsingBlock:^(FileResult * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [this rep:[obj content]];
        }];
        [self rep:[[NSString alloc] initWithFormat:@"(println \"#(ok %@)\")", [path lastPathComponent]]];
        return nil;
    };
    [[self env] setObject:[[JSFunction alloc] initWithFn:loadFile argCount:1 name:@"load-file/1"] forSymbol:[[JSSymbol alloc] initWithArity:1
                                                                                                                                     string:@"load-file"]];
}

/** Construct core lib file path. */
- (NSString *)coreLibPath:(NSString *)path {
    return [[NSString alloc] initWithFormat:@"%@/core.jsl", path];
}

/** Load core lib @c core.jsl from the search path. This paths includes the current working directory and the bundle directory in order. */
- (void)loadCoreLib {
    NSMutableArray *paths = [NSMutableArray new];
    [paths addObject:[self coreLibPath:[_fileOps currentDirectoryPath]]];
    [paths addObject:[self coreLibPath:[_fileOps bundlePath]]];
    [[_fileOps loadFileFromPath:paths isConcurrent:YES isLookup:YES] enumerateObjectsUsingBlock:^(FileResult * _Nonnull obj, NSUInteger idx,
                                                                                                  BOOL * _Nonnull stop) {
        [self rep:[obj content]];
    }];
}

#pragma mark Read

/** Converts the given string expression to AST */
- (NSMutableArray<id<JSDataProtocol>> *)read:(NSString *)string {
    return [_reader readString:string];
}

#pragma mark Eval

/** Evaluate the AST with the given environment. */
- (id<JSDataProtocol>)evalAST:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    if ([JSSymbol isSymbol:ast]) {
        return [env objectForSymbol:(JSSymbol *)ast];
    } else if ([JSList isList:ast]) {
        JSList *list = (JSList *)ast;
        NSUInteger count = [list count];
        NSUInteger i = 0;
        NSMutableArray *arr = [NSMutableArray new];
        if ([JSSymbol isSymbol:[list first]]) {
            [arr addObject:[self eval:[[JSSymbol alloc] initWithArity:count - 1 symbol:[list first]] withEnv:env]];
            i = 1;
        }
        for(; i < count; i = i + 1) {
            [arr addObject:[self eval:[list nth:i] withEnv:env]];
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

/** Checks if the given ast is either a non-empty list or vector. */
- (BOOL)isPair:(id<JSDataProtocol>)ast {
    return ([JSList isList:ast] && [(JSList *)ast count] > 0) || ([JSVector isVector:ast] && [(JSVector *)ast count] > 0);
}

/** Process quasiquote. */
- (id<JSDataProtocol>)quasiQuote:(id<JSDataProtocol>)ast {
    if (![self isPair:ast]) {
        id<JSDataProtocol>arg = ast;
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], arg] mutableCopy]];
    }
    JSList *lst = (JSList *)ast;
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

/** Checks if the given ast is list with macro at function position. */
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

/** Expands a macro call. */
- (id<JSDataProtocol>)macroExpand:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    while ([self isMacroCall:ast env:env]) {
        NSMutableArray *xs = [(JSList *)ast value];
        JSFunction *fn = (JSFunction *)[env objectForSymbol:[[JSSymbol alloc] initWithArity:[xs count] - 1 symbol:[xs first]]];
        ast = [fn apply:[xs rest]];
    }
    return ast;
}

/** Evaluate the expression with the given environment. */
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

#pragma mark Auto gensym

/** If the given symbol is present in the symbol table, updates the given symbol to match. */
- (JSSymbol * _Nullable)updateSymbol:(JSSymbol *)symbol table:(SymbolTable *)table {
    JSSymbol *aSym = [table symbol:symbol];
    if (aSym) {
        [symbol setValue:[aSym value]];
        return symbol;
    }
    return nil;
}

/** Updates bindings for key and value of the given hash map. */
- (JSHashMap * _Nullable)updateBindingsForHashMap:(JSHashMap *)ast table:(SymbolTable *)table {
    NSMapTable *dict = [ast value];
    NSArray *keys = [dict allKeys];
    NSUInteger len = [keys count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        dispatch_async(_queue, ^{
            [self updateBindingsForAST:keys[i] table:table];
        });
        dispatch_async(_queue, ^{
            [self updateBindingsForAST:[dict objectForKey:keys[i]] table:table];
        });
    }
    return ast;
}

/** Update only the symbols in the vector for which there is a match in the symbol table. */
- (JSVector *)updateBindingsForVector:(JSVector *)ast table:(SymbolTable *)table {
    [[ast value] enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id<JSDataProtocol>  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        if ([JSSymbol isSymbol:obj]) {
            [self updateSymbol:obj table:table];
        } else {
            [self updateBindingsForAST:obj table:table];
        }
    }];
    return ast;
}

- (JSList *)updateBindingsForList:(JSList *)ast table:(SymbolTable *)table {
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
                    // value part of the let*
                    [self updateBindingsForAST:bindings[j + 1] table:letTable];
                }
                table = letTable; // for the let scope
                i++;
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"def!"]) {
                if (!_isQuasiquoteMode) {
                    i++;
                    [table setSymbol:[ast nth:i]];  // Setting the def! bind name to symbol table
                }
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"defmacro!"]) {
                if (!_isQuasiquoteMode) {  // process defmacro! else, move to next symbol
                    i++;
                    [table setSymbol:[ast nth:i]];  // add binding name to main table
                    SymbolTable *macroTable = [[SymbolTable alloc] initWithTable:table];
                    i++;
                    [self updateBindingsForAST:[ast nth:i] table:macroTable];
                    return ast;
                }
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"quasiquote"]) {
                // In quasiquote mode, generate symbols unless unquote is found, where only lookup is to be performed
                i++;
                _quasiquoteDepth += 1;
                _isQuasiquoteMode = YES;
                // RULE: If no ~ or ~@ then no symbol lookup takes place - all are treated global. If ' encountered, no symbol lookup.
                id<JSDataProtocol> symForm = [self updateBindingsForAST:[ast nth:i] table:table];
                _quasiquoteDepth -= 1;
                if (_quasiquoteDepth == 0) _isQuasiquoteMode = NO;
                [ast update:symForm atIndex:i];
                continue;
            } else if ([sym position] == 0 && ([sym isEqualToName:@"unquote"] || [sym isEqualToName:@"splice-unquote"])) {
                // RULE: unquote and splice-unquote evaluates when the depth matches quasiquote. If quasiquote is more, evaluation does not happen, but symbol
                // lookup takes place.
                if (_isQuasiquoteMode) {
                    NSMutableArray *arr = [ast value];
                    [arr enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                        if (idx != 0) {
                            [self updateUnquoteBindingsForAST:obj table:table];
                        }
                    }];
                } else {
                    [[[JSError alloc] initWithFormat:MacroSymbolNotFound, [sym name]] throw];
                }
            } else if ([sym position] == 0 && [sym isEqualToName:@"fn*"]) {
                SymbolTable *fnTable = [[SymbolTable alloc] initWithTable:table];
                i++;
                JSList* elem = [ast nth:i];  // fn arguments
                NSMutableArray *arr = [elem value];
                NSMutableArray *symArgs = [arr mutableCopy];;
                NSUInteger len = [arr count];
                NSUInteger i = 0;
                for (i = 0; i < len; i++) {
                    id<JSDataProtocol> arg = arr[i];
                    if ([JSSymbol isSymbol:arg]) {
                        JSSymbol *aSym = (JSSymbol *)arg;
                        if ([[aSym value] isEqualToString:@"unquote"] || [[aSym value] isEqualToString:@"unquote-splice"]) {
                            i++;
                            arg = arr[i];
                            [self updateUnquoteBindingsForAST:arg table:table];
                        } else if ([[aSym value] isNotEqualTo:@"&"]) {
                            arg = [aSym autoGensym];
                            [fnTable setSymbol:arg];
                        }
                    }
                    [symArgs setObject:arg atIndexedSubscript:i];
                }
                table = fnTable;
                continue;
            } else if ([sym position] == 0 && [_keywords containsObject:[sym name]]) {
                continue;
            } else {
                // Update symbol in the list if there is a binding found
                [self updateSymbol:[ast nth:i] table:table];
            }
        } else {
            // element is not a symbol
            [self updateBindingsForAST:elem table:table];
        }
    }
    return ast;
}

/** Update symbol bindings if found, when list functions at 0th place which doesn't match the special forms, or for other symbols in other positions. */
- (JSSymbol *)updateBindingsForSymbol:(JSSymbol *)symbol table:(SymbolTable *)table {
    [self updateSymbol:symbol table:table];
    return symbol;
}

- (id<JSDataProtocol>)updateBindingsForAST:(id<JSDataProtocol>)xs table:(SymbolTable *)table {
    if ([JSList isList:xs]) {
        [self updateBindingsForList:xs table:table];
    } else if ([JSVector isVector:xs]) {
        [self updateBindingsForVector:xs table:table];
    } else if ([JSHashMap isHashMap:xs]) {
        [self updateBindingsForHashMap:xs table:table];
    } else if ([JSSymbol isSymbol:xs]) {
        [self updateBindingsForSymbol:xs table:table];
    }
    return xs;
}

#pragma mark Quasiquote

/**
 Updates binding when an unquote is found within quasiquote.

 `(let* [x 3] `(let* [x 1] (+ x (first [1 2 3]))))
 `(let* [x 3] `(let* [x 1] (+ x (first [1 2 3])))) ; -> `(let* [x 1] (+ x (first [1 2 3]))) ; -> 2
 */
- (JSList *)updateUnqoteBindingsForList:(JSList *)xs table:(SymbolTable *)table {
    if (_isQuasiquoteMode) {
        NSMutableArray *arr = [xs value];
        NSUInteger count = [arr count];
        NSUInteger i = 0;
        for (i = 0; i < count; i++) {
            id<JSDataProtocol> obj = arr[i];
            if ([JSSymbol isSymbol:obj]) {
                JSSymbol *sym = (JSSymbol *)obj;
                [self updateSymbol:sym table:table];  // Do symbol lookup only
            } else {
                [self updateUnquoteBindingsForAST:obj table:table];
            }
        };
    }
    return xs;
}

- (id<JSDataProtocol>)updateUnquoteBindingsForAST:(id<JSDataProtocol>)xs table:(SymbolTable *)table {
    if ([JSList isList:xs]) {
        [self updateUnqoteBindingsForList:xs table:table];
    } else if ([JSVector isVector:xs]) {
        [self updateBindingsForVector:xs table:table];
    } else if ([JSHashMap isHashMap:xs]) {
        [self updateBindingsForHashMap:xs table:table];
    } else if ([JSSymbol isSymbol:xs]) {
        [self updateBindingsForSymbol:xs table:table];
    }
    return xs;
}

#pragma mark Print

- (NSString *)print:(id<JSDataProtocol>)data {
    return [_printer printStringFor:data readably:YES];
}

#pragma mark REPL

/** Read-eval-print function. */
- (NSString * _Nullable)rep:(NSString *)string {
    NSMutableArray<id<JSDataProtocol>> *exps = [self read:string];
    NSUInteger len = [exps count];
    NSUInteger i = 0;
    NSString *ret = nil;
    for (i = 0; i < len; i++) {
        [self updateBindingsForAST:exps[i] table:_symTable];
        ret = [self print:[self eval:exps[i] withEnv:[self env]]];
    }
    // Symbol table contains symbols encountered which are defined using def!, defmacro!. Since processing of macros does not happen at this stage, any symbols
    // defined using macro functions other than defmacro! will not be added. Since these would be evaluated and added to the main env after each REP loop, we
    // can clear them.
    [_symTable clearAll];
    return ret;
}

#pragma mark Exception

/** Returns an expression from the given exception. */
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

/** Prints the given exception details to stdout. */
- (NSString * _Nullable)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably {
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
