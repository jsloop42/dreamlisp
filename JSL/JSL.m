//
//  JSL.m
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "JSL.h"

static NSString *coreLibFileName = @"core.jsl";
static NSString *hostLangVersion;
static NSString *langVersion;
static NSUInteger repTimeout = 10;

@implementation JSL {
    Reader *_reader;
    Printer *_printer;
    Env *_globalEnv;
    /** Current env */
    Env *_env;
    Core *_core;
    FileOps *_fileOps;
    NSArray *_keywords;
    BOOL _isQuasiquoteMode;
    NSUInteger _quasiquoteDepth;
    NSUInteger _unquoteDepth;
    dispatch_queue_t _queue;
    dispatch_queue_t _repQueue;  // Used for REPL queue for processing symbols from symbol table
    dispatch_group_t _dispatchGroup;
    BOOL _isREPL;  // Is running a REPL
}

@synthesize globalEnv = _globalEnv;
@synthesize env = _env;
@synthesize isREPL = _isREPL;

+ (void)initialize {
    hostLangVersion = [[NSString alloc] initWithFormat:@"%@ %.01f", @"Objective-C", (double)OBJC_API_VERSION];
    langVersion = [[NSString alloc] initWithFormat:@"JSL v%@ [%@]", JSLVersion, hostLangVersion];
}

- (instancetype)initWithREPL{
    self = [super init];
    if (self) {
        _isREPL = YES;
        [self bootstrap];
        [self printVersion];
        [self loadCoreLib];
    }
    return self;
}

- (instancetype)initWithoutREPL {
    self = [super init];
    if (self) {
        _isREPL = NO;
        [self bootstrap];
        [self loadCoreLib];
    }
    return self;
}

- (instancetype)init {
    self = [super init];
    return self;
}

- (void)bootstrap {
    _reader = [Reader new];
    _printer = [Printer new];
    _core = [Core new];
    _env = [[Env alloc] initWithModuleName:defaultModuleName isUserDefined:NO];
    // Add modules to module table
    [self setModule:_env];  // default module
    [self setModule:[_core env]];  // core module
    _globalEnv = _env;
    _fileOps = [FileOps new];
    _isQuasiquoteMode = NO;
    _quasiquoteDepth = 0;
    _queue = dispatch_queue_create("jsl-dispatch-queue", nil);
    _repQueue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
    _keywords = @[@"fn*", @"if", @"do", @"quote", @"quasiquote", @"unquote", @"splice-unquote", @"macroexpand", @"try*", @"catch*"];
    [self setLoadFileToREPL];
    [self setEvalToREPL];
}

#pragma mark Env setup

/** Add @c eval function to the environment. */
- (void)setEvalToREPL{
    JSL * __weak weakSelf = self;
    id<JSDataProtocol>(^fn)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        JSL *this = weakSelf;
        id<JSDataProtocol> ast = (id<JSDataProtocol>)arg[0];
        return [self eval:[self updateBindingsForAST:ast table:[this->_env symbolTable]] withEnv:[self env]];
    };
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
        NSMutableArray<FileResult *> *files = [this->_fileOps loadFileFromPath:[@[path] mutableCopy] isConcurrent:NO isLookup:NO];
        if ([files count] == 0) {
            info(@"%@ %@", @"Error loading", path);
            return nil;
        } else {
            Env *current = this->_env;
            BOOL __block hasError = NO;
            [files enumerateObjectsUsingBlock:^(FileResult * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                 @try {
                     [this rep:[obj content]];
                 } @catch (NSException *exception) {
                     hasError = YES;
                     [this printException:exception log:YES readably:YES];
                 }
            }];
            this->_env = current;
            [this updateModuleName:[current moduleName]];
            if (!hasError) info(@"%@", [[NSString alloc] initWithFormat:@"#(ok %@)", [path lastPathComponent]]);
        }
        return [JSNil new];
    };
    [[self env] setObject:[[JSFunction alloc] initWithFn:loadFile argCount:1 name:@"load-file/1"] forSymbol:[[JSSymbol alloc] initWithArity:1
                                                                                                                                     string:@"load-file"]];
}

/** Construct core lib file path. */
- (NSString *)coreLibPath:(NSString *)path {
    return [[NSString alloc] initWithFormat:@"%@/%@", path, coreLibFileName];
}

/** Load core lib @c core.jsl from the search path. This paths includes the current working directory and the bundle directory in order. */
- (void)loadCoreLib {
    NSMutableArray *paths = [NSMutableArray new];
    [paths addObject:[self coreLibPath:[_fileOps currentDirectoryPath]]];
    [paths addObject:[self coreLibPath:[_fileOps bundlePath]]];
    NSMutableArray<FileResult *> *files = [_fileOps loadFileFromPath:paths isConcurrent:YES isLookup:YES];
    if ([files count] == 0) {
        info(@"%@ %@", @"Error loading", coreLibFileName);
    } else {
        _env = [_core env];
        [self updateModuleName:[_env moduleName]];
        [files enumerateObjectsUsingBlock:^(FileResult * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            @try {
                [self rep:[obj content]];
            } @catch (NSException *exception) {
                [self printException:exception log:YES readably:YES];
            }
        }];
        _env = _globalEnv;
        [self updateModuleName:[_env moduleName]];
    }
}

- (void)printVersion {
    if (_isREPL) info(@"%@\n", langVersion);
}

#pragma mark Module

- (void)setModule:(Env *)moduleEnv {
    [Env setEnv:moduleEnv forModuleName:[moduleEnv moduleName]];
}

- (Env * _Nullable)module:(NSString *)name {
    return [Env envForModuleName:name];
}

- (void)removeModule:(NSString *)name {
    [Env removeModule:name];
    _env = _globalEnv;
    [self updateModuleName:[_env moduleName]];
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
            // Get the function correspoding to the symbol
            JSSymbol *sym = (JSSymbol *)[list first];
            [arr addObject:[self eval:[[JSSymbol alloc] initWithArity:count - 1 position:[sym position] symbol:sym] withEnv:env]];
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
- (id<JSDataProtocol>)quasiquote:(id<JSDataProtocol>)ast {
    if (![self isPair:ast]) {
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], ast] mutableCopy]];
    }
    JSList *lst = (JSList *)ast;
    NSMutableArray *xs = [lst value];
    id<JSDataProtocol> first = [xs first];
    if ([JSSymbol isSymbol:first] && [[(JSSymbol *)first name] isEqual:@"unquote"]) return [xs second];
    if ([self isPair:first]) {
        if ([JSVector isVector:first] && [xs count] == 1) {
            NSMutableArray *arr = [NSMutableArray new];
            [arr addObject:[[JSSymbol alloc] initWithName:@"vector"]];
            [arr addObjectsFromArray:[(JSVector *)first value]];
            // (quote (vector 1 2 3)) -> JSList:(quote JSList:(vector 1 2 3))
            return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"list"], [[JSList alloc] initWithArray:arr]] mutableCopy]];
        }
        NSMutableArray *list = [(JSList *)first value];
        if (![list isEmpty] && [JSSymbol isSymbol:[list first]] && [[(JSSymbol *)[list first] name] isEqual:@"splice-unquote"]) {
            return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"concat"], [list second],
                                                    [self quasiquote:[[JSList alloc] initWithArray:[xs rest]]]] mutableCopy]];
        }
    }
    return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"cons"], [self quasiquote:first],
                                            [self quasiquote:[[JSList alloc] initWithArray:[xs rest]]]] mutableCopy]];
}

/** Checks if the given ast is list with macro at function position. */
- (BOOL)isMacroCall:(id<JSDataProtocol>)ast env:(Env *)env {
    if ([JSList isList:ast]) {
        NSMutableArray *xs = [(JSList *)ast value];
        id<JSDataProtocol> first = [xs first];
        if (first && [JSSymbol isSymbol:first]) {
            JSSymbol *sym = [[JSSymbol alloc] initWithArity:[xs count] - 1 position:0 symbol:first];
            id<JSDataProtocol> fnData = [env objectForSymbol:sym isThrow:NO];
            //[xs update:sym atIndex:0];  // The symbol will be updated with props, so use the updated symbol. FIXME: seems to be not required
            if (fnData && [JSFunction isFunction:fnData]) return [(JSFunction *)fnData isMacro];
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
                        // Uses current env `[self env]` than the passed in `env` param so that the closure gets the latest env variable.
                        Env *fnEnv = [[Env alloc] initWithEnv:[self env] binds:[(JSList *)[xs second] value] exprs:arg];
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
                    ast = [self quasiquote:exp];
                    continue;
                } else if ([[sym name] isEqual:@"macroexpand"]) {
                    return [self macroExpand:[xs second] withEnv:env];
                } else if ([[sym name] isEqual:@"defmodule"]) {  // module
                    return [self defineModule:ast];
                } else if ([[sym name] isEqual:@"in-module"]) {
                    return [self changeModule:ast];
                } else if ([[sym name] isEqual:@"remove-module"]) {
                    [self removeModule:[(JSSymbol *)[(JSList *)ast second] name]];
                    return [JSNil new];
                }
            }
            // Function
            NSMutableArray *list = [(JSList *)[self evalAST:ast withEnv:env] value];
            JSFunction *fn = [JSFunction dataToFunction:[list first]];
            // The symbol in the ast at first position is resolved to corrsponding function in the list, which is in `fn` variable.
            // NB: Any value properties can be update from the corresponding symbol at this position.
            JSSymbol *bind = (JSSymbol *)[(JSList *)ast first];
            if ([fn isImported]) {  // case where the inner function ast is imported but not marked in the symbol
                if (![bind isImported]) {
                    [bind setIsImported:YES];
                    [bind setIsQualified:YES];
                    [bind setInitialModuleName:[fn moduleName]];
                }
            } else if ([bind isImported]) {
                [fn setIsImported:YES];  // case where the symbol is resolved from fault and the corresponding value's props are yet to be set
            }
            NSMutableArray *rest = [list rest];  // The arguments to the function
            if ([fn ast]) {
                ast = [fn ast];
                env = [[Env alloc] initWithEnv:[fn env] binds:[fn params] exprs:rest isImported:[fn isImported] currentEnv:env];
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

#pragma mark Module AST

/**
 (defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))
 (defmodule foo (export (greet 0)) (import (from bar (sum 1))))
 (defmodule foo (export (greet 0)) (export all) (import (from bar (sum 1))))
 */
- (JSSymbol *)defineModule:(id<JSDataProtocol>)ast {
    JSList *xs = (JSList *)ast;
    JSSymbol *modSym = [xs second];
    [modSym setIsModule:YES];
    NSString *modName = [modSym name];
    Env *modEnv = [[Env alloc] initWithModuleName:modName isUserDefined:YES];
    [self setModule:modEnv];
    _env = modEnv; // change env to current module
    [self updateModuleName:modName];
    // The third element onwards are imports and exports
    [self processModuleDirectives:[xs drop:2] module:_env];
    if (_isREPL) prompt = [[modName stringByAppendingString:@"> "] UTF8String];
    return modSym;
}

/** Process module import exports. The ast can be of the form (export (a 1) (b 0)) (export (c 2) (d 1)) (import (from list (all 2)) (from io (read 1))) .. */
- (void)processModuleDirectives:(JSList *)ast module:(Env *)env {
    NSMutableArray *arr = [ast value];
    NSLock *lock = [NSLock new];
    [arr enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id  _Nonnull obj, NSUInteger i, BOOL * _Nonnull stop) {
        JSList *xs = (JSList *)obj;
        id<JSDataProtocol> modDir = [xs first];
        if ([JSSymbol isSymbol:modDir withName:@"export"]) {
            JSList *elem = [xs rest];
            if ([elem count] == 1 && [JSSymbol isSymbol:[elem first] withName:@"all"]) {
                [env setIsExportAll:YES];
                [[env module] removeAllObjects];
                *stop = YES;
            } else {
                [env setIsExportAll:NO];
                NSMutableArray *fnList = [(JSList *)elem value];
                [fnList enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(id  _Nonnull expObj, NSUInteger j, BOOL * _Nonnull expStop) {
                    NSMutableArray *aExp = (NSMutableArray *)[(JSList *)expObj value];
                    JSSymbol *sym = (JSSymbol *)[aExp first];
                    JSNumber *arityNum = (JSNumber *)[aExp second];
                    NSInteger arity = [arityNum integerValue];
                    [sym setArity:arity];
                    [sym setInitialArity:arity];
                    [sym updateArity];
                    [sym setIsFault:YES];
                    [sym setModuleName:[env moduleName]];
                    [sym setInitialModuleName:[env moduleName]];
                    [lock lock];
                    [[env module] setObject:[[JSFault alloc] initWithModule:[env moduleName] isImportFault:NO] forSymbol:sym];
                    [lock unlock];
                }];
            }
        } else if ([JSSymbol isSymbol:modDir withName:@"import"]) {
            JSList *imports = (JSList *)[xs rest];
            NSMutableArray *impArr = (NSMutableArray *)[imports value];
            [impArr enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                JSList *imp = (JSList *)obj;
                if ([JSSymbol isSymbol:[imp first] withName:@"from"]) {
                    NSString *modName = [(JSSymbol *)[imp second] value];
                    Env *impEnv = [Env envForModuleName:modName];
                    if (impEnv) {
                        NSMutableArray *impFns = (NSMutableArray *)[(JSList *)[imp drop:2] value];
                        [impFns enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                            JSList *aExp = (JSList *)obj;
                            JSSymbol *sym = (JSSymbol *)[aExp first];
                            JSNumber *arityNum = (JSNumber *)[aExp second];
                            NSInteger arity = [arityNum integerValue];
                            [sym setArity:arity];
                            [sym setInitialArity:arity];
                            [sym updateArity];
                            [sym setIsFault:YES];
                            [sym setModuleName:[env moduleName]];
                            [sym setInitialModuleName:modName];
                            [sym setIsImported:YES];
                            [lock lock];
                            [[env table] setObject:[[JSFault alloc] initWithModule:modName isImportFault:YES] forKey:sym];
                            [lock unlock];
                        }];
                    }
                }
            }];
        }
    }];
}

/** Change current module to the given one. */
- (JSSymbol *)changeModule:(id<JSDataProtocol>)ast {
    JSList *xs = (JSList *)ast;
    JSSymbol *modSym = [xs second];
    [modSym setIsModule:YES];
    NSString *modName = [modSym name];
    if ([modName isEqual:defaultModuleName]) {
        _env = _globalEnv;
    } else {
        // check modules table
        Env *modEnv = [self module:modName];
        if (modEnv) {
            _env = modEnv;
        } else {
            [[[JSError alloc] initWithFormat:ModuleNotFound, modName] throw];
        }
    }
    [self updateModuleName:modName];
    return modSym;
}

/** Changes the prompt if in REPL and updates the @c currentModuleName */
- (void)updateModuleName:(NSString *)moduleName {
    if (_isREPL) prompt = [[moduleName stringByAppendingString:@"> "] UTF8String];
    currentModuleName = moduleName;
}

#pragma mark Auto gensym

- (JSSymbol * _Nullable)symbol:(JSSymbol *)symbol arity:(NSInteger)arity fromTable:(SymbolTable *)table {
    JSSymbol *sym = [table symbol:symbol];
    if (sym) return sym;
    if ([symbol position] == 0 && [symbol arity] != arity && ![symbol hasNArity]) {  // TODO: is position check required?
        [symbol setArity:arity];
        [symbol updateArity];
        return [self symbol:symbol arity:arity fromTable:table];
    }
    if (![symbol hasNArity]) {
        [symbol toNArity];
        return [self symbol:symbol arity:arity fromTable:table];
    }
    [symbol resetArity];
    return [table outer] ? [self symbol:symbol arity:arity fromTable:[table outer]] : nil;
}

- (JSSymbol * _Nullable)findSymbol:(JSSymbol *)symbol arity:(NSInteger)arity table:(SymbolTable *)table {
    JSSymbol *sym = nil;
    NSString *modName = [symbol moduleName];
    if ([symbol isQualified]) {
        sym = [self symbol:symbol arity:arity fromTable:[[Env envForModuleName:modName] symbolTable]];
        return sym;
    }
    if ([modName isEqual:currentModuleName] && [modName isNotEqualTo:coreModuleName]) {  // Symbol from the current module
        sym = [self symbol:symbol arity:arity fromTable:table];
        if (sym) return sym;
    }
    if ([modName isNotEqualTo:currentModuleName]) {  // current module is "foo", symbol's module name is "user" which is default, and symbol belongs to "core", for eg: +
        sym = [self symbol:symbol arity:arity fromTable:[[Env envForModuleName:modName] symbolTable]];
        if (sym) return sym;
    }
    // check current module
    sym = [self symbol:symbol arity:arity fromTable:[[Env envForModuleName:coreModuleName] symbolTable]];
    if (sym) return sym;
    // Check core module
    [symbol setModuleName:coreModuleName];
    sym = [self symbol:symbol arity:arity fromTable:[[Env envForModuleName:coreModuleName] symbolTable]];
    if (sym) return sym;
    [symbol setModuleName:modName];
    sym = [table symbol:symbol];
    if (sym) return sym;
    return [table outer] ? [self findSymbol:symbol arity:arity table:[table outer]] : nil;
}

/** If the given symbol is present in the symbol table, updates the given symbol to match. */
- (void)updateSymbol:(JSSymbol *)symbol arity:(NSInteger)arity table:(SymbolTable *)table {
    JSSymbol *sym = nil;
    NSString *modName = [symbol moduleName];
    if (![symbol isQualified] && [modName isNotEqualTo:currentModuleName]) [symbol setModuleName:currentModuleName];
    sym = [self findSymbol:symbol arity:arity table:table];
    if (sym) [self updateGensymProps:sym forSymbol:symbol];
    // Symbol does not belong to current module, set back to the initial module name and do another lookup
    if (![symbol isQualified] && [modName isNotEqualTo:currentModuleName]) [sym setModuleName:modName];
    sym = [self findSymbol:symbol arity:arity table:table];
    if (sym) [self updateGensymProps:sym forSymbol:symbol];
}

- (void)updateGensymProps:(JSSymbol *)gensym forSymbol:(JSSymbol *)symbol {
    if ([[gensym name] isNotEqualTo:[symbol name]]) {
        [symbol setValue:[gensym value]]; // update only the name as we are matching gensym values only.
        // (defmodule foo (export (greet 0)) (import (from bar (sum 1))))
        // (def! greet (fn* () (sum 32)))
        if ([symbol isQualified]) [symbol setInitialModuleName:[symbol moduleName]];
        [symbol setInitialArity:[gensym arity]];
        [symbol resetArity];
        //[symbol copyProperties:gensym];  //
    }
}

//- (JSSymbol * _Nullable)updateSymbol:(JSSymbol *)symbol arity:(NSInteger)arity table:(SymbolTable *)table {
//    JSSymbol *aSym = nil;
//    NSString *modName = [symbol moduleName];
//    if ([modName isEqual:currentModuleName]) {  // Symbol from the current module
//        aSym = [table symbol:symbol];
//        if (aSym) {
//            [symbol setValue:[aSym value]];
//            return symbol;
//        } else if ([symbol arity] != -2) {
//            [symbol setArity:-2];
//            aSym = [table symbol:symbol];
//            if (aSym) {
//                [symbol setValue:[aSym value]];
//                [symbol setArity:[symbol initialArity]];
//                return symbol;
//            }
//        } else {
//            // check core module  // TODO: refactor out
//            [symbol setModuleName:coreModuleName];
//            aSym = [[[Env envForModuleName:coreModuleName] symbolTable] symbol:symbol];
//            if (aSym) {
//                [symbol setValue:[aSym value]];
//                return symbol;
//            } else {
//                [symbol toNArity];
//                aSym = [[[Env envForModuleName:coreModuleName] symbolTable] symbol:symbol];
//                if (aSym) {
//                    [symbol setValue:[aSym value]];
//                    return symbol;
//                }
//                [symbol resetArity];
//            }
//            [symbol setModuleName:modName];
//        }
//    } else {  // Symbol is from another module
//        Env *env = [Env envForModuleName:modName];
//        aSym = [[env symbolTable] symbol:symbol];
//        if (aSym) {
//            [symbol setValue:[aSym value]];
//            return symbol;
//        }
//    }
//    return nil;
//}

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
//            [self updateSymbol:obj table:table];
            [self updateSymbol:obj arity:-2 table:table];
        } else {
            [self updateBindingsForAST:obj table:table];
        }
    }];
    return ast;
}

- (void)setPropsNameForBinding:(JSSymbol *)symbol {
    if ([[symbol initialModuleName] isEqual:defaultModuleName]) {  // TODO: remove this check?
        // The module names default to "user". So we need to update it to current module where the function or let binding is encountered.
        [symbol setInitialModuleName:currentModuleName];
        [symbol setModuleName:currentModuleName];
    }
}

- (JSList *)updateBindingsForList:(JSList *)ast table:(SymbolTable *)table {
    NSUInteger len = [ast count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        id<JSDataProtocol> elem = [ast nth:i];
        if ([JSSymbol isSymbol:elem]) {
            JSSymbol *sym = (JSSymbol *)elem;
            if ([sym position] == 0 && [sym isEqualToName:@"let*"]) {  // (let* exp)
                //if (!_isQuasiquoteMode) {
                    SymbolTable *letTable = [[SymbolTable alloc] initWithTable:table];
                    NSMutableArray *bindings = [(JSList *)[ast nth:i + 1] value]; // bindings -> list or vector (let* (x 1 y 2) ..)
                    NSUInteger j = 0;
                    NSUInteger blen = [bindings count];
                    // Check if any of the symbols are redefined
                    id<JSDataProtocol> aSym = nil;
                    for (j = 0; j < blen; j += 2) {
                        // NOTE: pattern matching can be done here for let destructuring
                        aSym = bindings[j];
                        JSSymbol *elem = (JSSymbol *)aSym;
                        if ([[aSym value] isEqualToString:@"unquote"] || [[aSym value] isEqualToString:@"unquote-splice"]) {
                            j++;
                            aSym = bindings[j];
                            [self updateUnquoteBindingsForAST:aSym table:table];
                        } else {
                            [elem autoGensym];  // let* binding symbols are gensymed if not found within unquote or unquote-splice
                            [letTable setSymbol:elem];
                            // value part of the let*
                            [self updateBindingsForAST:bindings[j + 1] table:letTable];
                        }
//                        if ([JSSymbol isSymbol:aSym]) {
//                            JSSymbol *elem = (JSSymbol *)aSym;
//                            [elem autoGensym];  // let* binding symbols are gensymed always
//                            [letTable setSymbol:elem];
//                        }
                    }
                    table = letTable; // for the let scope
                    i++;
                //}
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"def!"]) {
                if (!_isQuasiquoteMode) {
                    i++;
                    JSSymbol *bind = [ast nth:i];
                    [JSSymbol updateProperties:bind list:[ast nth:2]];
                    [table setSymbol:bind];  // Setting the def! bind name to symbol table
                }
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"defmacro!"]) {
                if (!_isQuasiquoteMode) {  // process defmacro! else, move to next symbol
                    SymbolTable *macroTable = [[SymbolTable alloc] initWithTable:table];  // This is creating a new scope to make gensyms unique
                    i++;
                    JSSymbol *bind = [ast nth:i];
                    [JSSymbol updateProperties:bind list:[ast nth:2]];
                    [table setSymbol:bind];  // add binding name to main table
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
                //NSMutableArray *symArgs = [arr mutableCopy];;
                NSUInteger len = [arr count];
                NSUInteger i = 0;
                for (i = 0; i < len; i++) {
                    id<JSDataProtocol> arg = arr[i];  // mutable
                    if ([JSSymbol isSymbol:arg]) {  // Process function arguments
                        JSSymbol *aSym = (JSSymbol *)arg;
                        // Handle nested fn* within quasiquote
                        if ([[aSym value] isEqualToString:@"unquote"] || [[aSym value] isEqualToString:@"unquote-splice"]) {
                            i++;
                            arg = arr[i];
                            [self setPropsNameForBinding:arg];
                            [self updateUnquoteBindingsForAST:arg table:table];
                        } else if ([[aSym value] isNotEqualTo:@"&"]) {
                            [self setPropsNameForBinding:aSym];
                            arg = [aSym autoGensym];
                            [fnTable setSymbol:arg];
                        }
                    }
                    //[symArgs setObject:arg atIndexedSubscript:i];
                }
                table = fnTable;
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"defmodule"]) {
                if (!_isQuasiquoteMode) {
                    return ast;
                }
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"export"]) {
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"import"]) {
                //TODO
                // (import (net.jsloop.tree tree) (create-tree 0) (right-node 1) (left-node 1))
                // (import net.jsloop.tree (create-tree 0) (right-node 1) (left-node 1))
            } else if ([sym position] == 0 && [sym isEqualToName:@"in-module"]) {
                continue;
            } else if ([sym position] == 0 && [sym isEqualToName:@"remove-module"]) {
                continue;
            } else if ([sym position] == 0 && [_keywords containsObject:[sym name]]) {
                continue;
            } else {
                // Update symbol in the list if there is a binding found
//                if ([sym position] == 0) {  // ((fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)
//                    [sym setInitialArity:[ast count] - 1];
//                    [sym resetArity];
//                }
                //[self updateSymbol:[ast nth:i] table:table];  // TODO: nth ?
                //[self updateSymbol:sym table:table];
                [self updateSymbol:sym arity:[ast count] - 1 table:table];
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
    //[self updateSymbol:symbol table:table];
    [self updateSymbol:symbol arity:[symbol arity] table:table];
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
                //[self updateSymbol:sym table:table];  // Do symbol lookup only
                [self updateSymbol:sym arity:[sym arity] table:table];
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

- (void)updateEnvFromSymbolTable:(SymbolTable *)table env:(Env *)env callback:(void (^)(void))callback {
    NSString *modName = [env moduleName];
    [[[table table] allKeys] enumerateObjectsWithOptions:0 usingBlock:^(id  _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        if ([modName isEqual:defaultModuleName] || [modName isEqual:coreModuleName] ||  [env isExportAll]) {
            [[env module] setObject:[table symbol:obj] forSymbol:obj];
        } else {
            [[env table] setObject:[table symbol:obj] forKey:obj];
        }
    }];
    //[table removeAllObjects];  // TODO: ?
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
    //_dispatchGroup = dispatch_group_create();
    for (i = 0; i < len; i++) {
        [self updateBindingsForAST:exps[i] table:[_env symbolTable]];  // Symbol table contains symbols encountered which are defined using def!, defmacro!.
      //  info(@"%@", [_env symbolTable]);
        [self updateEnvFromSymbolTable:[_env symbolTable] env:_env callback:nil];
        ret = [self print:[self eval:exps[i] withEnv:[self env]]];
    }
    //[[_env symbolTable] removeAllObjects];
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
