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

@implementation JSL {
    Reader *_reader;
    Printer *_printer;
    Env *_globalEnv;
    /** Current env */
    Env *_env;
    Core *_core;
    FileOps *_fileOps;
    BOOL _isQuasiquoteMode;
    NSUInteger _quasiquoteDepth;
    NSUInteger _unquoteDepth;
    dispatch_queue_t _queue;
    dispatch_queue_t _repQueue;  // Used for REPL queue for processing symbols from symbol table
    BOOL _isREPL;  // Is running a REPL
    NSString *_prompt;
}

@synthesize reader = _reader;
@synthesize globalEnv = _globalEnv;
@synthesize env = _env;
@synthesize isREPL = _isREPL;
@synthesize prompt = _prompt;

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
    _env = [[Env alloc] initWithModuleName:[Const defaultModuleName] isUserDefined:NO];
    [_env setModuleDescription:[Const defaultModuleDescription]];
    [State setCurrentModuleName:[_env moduleName]];
    // Add modules to module table
    [self addModule:_env];  // default module
    [self addModule:[_core env]];  // core module
    _globalEnv = _env;
    _fileOps = [FileOps new];
    _isQuasiquoteMode = NO;
    _quasiquoteDepth = 0;
    _queue = dispatch_queue_create("jsl-dispatch-queue", nil);
    _repQueue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
    [self setLoadFileToREPL];
    [self setEvalToREPL];
    if (_isREPL) _prompt = [[State currentModuleName] stringByAppendingString:@"> "];
}

#pragma mark Env setup

/** Add @c eval function to the environment. */
- (void)setEvalToREPL{
    JSL * __weak weakSelf = self;
    id<JSDataProtocol>(^fn)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        JSL *this = weakSelf;
        return [self eval:[arg first] withEnv:[this env]];
    };
    NSString *coreModuleName = [Const coreModuleName];
    Env *coreEnv = [Env forModuleName:coreModuleName];
    [coreEnv setObject:[[JSFunction alloc] initWithFn:fn argCount:1 name:@"eval/1"]
                forKey:[[JSSymbol alloc] initWithArity:1 string:@"eval" moduleName:coreModuleName]];
    [coreEnv setObject:[JSList new] forKey:[[JSSymbol alloc] initWithName:@"*ARGV*" moduleName:coreModuleName]];
    [coreEnv setObject:[[JSString alloc] initWithFormat:@"%@", hostLangVersion]
                forKey:[[JSSymbol alloc] initWithName:@"*host-language*" moduleName:coreModuleName]];
    [coreEnv setObject:[[JSString alloc] initWithFormat:@"%@", langVersion] forKey:[[JSSymbol alloc] initWithName:@"*version*" moduleName:coreModuleName]];
    [coreEnv setObject:[JSList new] forKey:[[JSSymbol alloc] initWithName:@"*ARGV*" moduleName:coreModuleName]];
}

/** Add @c load-file to the environment, which loads and evaluates the expressions contained in the file. */
- (void)setLoadFileToREPL {
    JSL * __weak weakSelf = self;
    id<JSDataProtocol>(^loadFile)(NSMutableArray *arg) = ^id<JSDataProtocol>(NSMutableArray *arg) {
        JSL *this = weakSelf;
        NSString *path = [[JSString dataToString:arg[0] fnName:@"load-file/1"] value];
        NSMutableArray<FileResult *> *files = [this->_fileOps loadFileFromPath:[@[path] mutableCopy] isConcurrent:NO isLookup:NO];
        if ([files count] == 0) {
            error(@"%@ %@", @"Error loading", path);
            return nil;
        } else {
            BOOL __block hasError = NO;
            [files enumerateObjectsUsingBlock:^(FileResult * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                 @try {
                     [this rep:[obj content]];
                 } @catch (NSException *exception) {
                     hasError = YES;
                     [this printException:exception log:YES readably:YES];
                 }
            }];
            [this changeModuleTo:[Const defaultModuleName]];
            if (!hasError && [this isREPL]) info(@"%@", [[NSString alloc] initWithFormat:@"#(ok %@)", [path lastPathComponent]]);
        }
        return [JSNil new];
    };
    Env *coreEnv = [Env forModuleName:[Const coreModuleName]];
    JSSymbol *sym = [[JSSymbol alloc] initWithArity:1 string:@"load-file" moduleName:[Const coreModuleName]];
    JSFunction *fn = [[JSFunction alloc] initWithFn:loadFile argCount:1 name:@"load-file/1"];
    [fn setModuleName:[Const coreModuleName]];
    [[coreEnv exportTable] setObject:fn forKey:sym];
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
    NSString *moduleName = [Const coreModuleName];
    if ([files count] == 0) {
        error(@"%@ %@", @"Error loading", coreLibFileName);
    } else {
        _env = [_core env];
        [_reader setModuleName:moduleName];
        [self updateModuleName:moduleName];
        [files enumerateObjectsUsingBlock:^(FileResult * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            @try {
                [self rep:[obj content]];
            } @catch (NSException *exception) {
                [self printException:exception log:YES readably:YES];
            }
        }];
        _env = _globalEnv;
        moduleName = [_env moduleName];
        [self updateModuleName:moduleName];
        [_reader setModuleName:moduleName];
    }
}

- (void)printVersion {
    if (_isREPL) info(@"%@\n", langVersion);
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
        return [env objectForKey:(JSSymbol *)ast];
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
        id<JSDataProtocol>arg = ast;
        return [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithName:@"quote"], arg] mutableCopy]];
    }
    JSList *lst = (JSList *)ast;
    NSMutableArray *xs = [lst value];
    id<JSDataProtocol> first = [xs first];
    if ([JSSymbol isSymbol:first] && [[(JSSymbol *)first value] isEqual:@"unquote"]) return [xs second];
    if ([self isPair:first]) {
        NSMutableArray *list = [(JSList *)first value];
        if (![list isEmpty] && [JSSymbol isSymbol:[list first]] && [[(JSSymbol *)[list first] value] isEqual:@"splice-unquote"]) {
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
            id<JSDataProtocol> fnData = [env objectForKey:sym isThrow:NO];
            if (fnData && [JSFunction isFunction:fnData]) return [(JSFunction *)fnData isMacro];
        }
    }
    return NO;
}

/** Expands a macro call. */
- (id<JSDataProtocol>)macroExpand:(id<JSDataProtocol>)ast withEnv:(Env *)env {
    while ([self isMacroCall:ast env:env]) {
        NSMutableArray *xs = [(JSList *)ast value];
        JSFunction *fn = (JSFunction *)[env objectForKey:[[JSSymbol alloc] initWithArity:[xs count] - 1 symbol:[xs first]]];
        ast = [fn apply:[xs rest]];
    }
    return ast;
}

- (JSList *)toDoForm:(NSMutableArray *)arr {
    [arr insertObject:[[JSSymbol alloc] initWithName:@"do"] atIndex:0];
    return [[JSList alloc] initWithArray:arr];
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
                if ([[sym value] isEqual:@"def"]) {
                    id<JSDataProtocol> val = [self eval:[xs nth:2] withEnv:env];
                    [env setObject:val forKey:[JSSymbol symbolWithArityCheck:[xs second] withObject:val]];
                    return val;
                } else if ([[sym value] isEqual:@"defmacro"]) {
                    NSMutableArray *args = [xs drop:2];
                    [args add:[[JSSymbol alloc] initWithName:@"fn"] atIndex:0];
                    JSList *fnList = [[JSList alloc] initWithArray: args];
                    JSFunction *fn = (JSFunction *)[self eval:fnList withEnv:env];
                    [fn setName:[(JSSymbol *)[xs second] value]];
                    JSFunction *macro = [[JSFunction alloc] initWithMacro:fn];
                    [env setObject:macro forKey:[JSSymbol symbolWithArityCheck:[xs second] withObject:macro]];
                    return macro;
                } else if ([[sym value] isEqual:@"try"]) {
                    NSMutableArray *form = [xs rest];
                    @try {
                        if ([JSList isList:[xs last]]) {
                            JSList *last = [xs last];
                            if ([JSSymbol isSymbol:[last first] withName:@"catch"]) {
                                form = [form dropLast];
                            }
                        }
                        return [self eval:[self toDoForm:form] withEnv:env];
                    } @catch (NSException *exception) {
                        if ([xs count] > 2) {
                            JSList *catchxs = (JSList *)[xs nth:2];
                            if ([JSSymbol isSymbol:[catchxs first]] && [[(JSSymbol *)[catchxs first] value] isNotEqualTo:@"catch"]) {
                                [[[JSError alloc] initWithData:[catchxs first]] throw];
                            }
                            Env *catchEnv = [[Env alloc] initWithEnv:env binds:[@[(JSSymbol *)[catchxs second]] mutableCopy]
                                                               exprs:[@[[self exceptionInfo:exception]] mutableCopy]];
                            return [self eval:[self toDoForm:[[catchxs drop:2] value]] withEnv:catchEnv];
                         }
                        @throw exception;
                    }
                } else if ([[sym value] isEqual:@"do"]) {
                    NSInteger i = 0;
                    NSInteger len = [xs count];
                    for (i = 1; i < len - 1; i++) {
                        [self eval:[xs nth:i] withEnv:env];
                    }
                    id<JSDataProtocol> last = [xs last];
                    ast = last ? last : [JSNil new];
                    continue;
                } else if ([[sym value] isEqual:@"if"]) {
                    id<JSDataProtocol> res = [self eval:[xs second] withEnv:env];
                    if ([JSNil isNil:res] || ([JSBool isBool:res] && [(JSBool *)res value] == NO)) {
                        ast = [xs count] > 3 ? [xs nth:3] : [JSNil new];
                    } else {
                        ast = [xs nth:2];
                    }
                    continue;
                } else if ([[sym value] isEqual:@"fn"]) {
                    JSList *form = (JSList *)[self toDoForm:[xs drop:2]];
                    id<JSDataProtocol>(^fn)(NSMutableArray *) = ^id<JSDataProtocol>(NSMutableArray * arg) {
                        Env *fnEnv = [[Env alloc] initWithEnv:env binds:[(JSList *)[xs second] value] exprs:arg];
                        return  [self eval:form withEnv:fnEnv];
                    };
                    return [[JSFunction alloc] initWithAst:form params:[(JSList *)[xs second] value] env:env macro:NO meta:nil fn:fn];
                } else if ([[sym value] isEqual:@"let"]) {
                    Env *letEnv = [[Env alloc] initWithEnv:env];
                    NSMutableArray *bindings = [JSVector isVector:[xs second]] ? [(JSVector *)[xs second] value] : [(JSList *)[xs second] value];
                    NSUInteger len = [bindings count];
                    NSUInteger i = 0;
                    for (i = 0; i < len; i += 2) {
                        id<JSDataProtocol> val = [self eval:[bindings nth: i + 1] withEnv:letEnv];
                        [letEnv setObject:val forKey:[JSSymbol symbolWithArityCheck:[bindings nth:i] withObject:val]];
                    }
                    ast = [self toDoForm:[xs drop:2]];
                    env = letEnv;
                    continue;
                } else if ([[sym value] isEqual:@"quote"]) {
                    return [xs second];
                } else if ([[sym value] isEqual:@"quasiquote"]) {
                    id<JSDataProtocol> exp = [xs second];
                    ast = [self quasiquote:exp];
                    continue;
                } else if ([[sym value] isEqual:@"macroexpand"]) {
                    return [self macroExpand:[xs second] withEnv:env];
                } else if ([[sym value] isEqual:@"defmodule"]) {  // module
                    return [self defineModule:ast];
                } else if ([[sym value] isEqual:@"in-module"]) {
                    return [self changeModule:ast];
                } else if ([[sym value] isEqual:@"remove-module"]) {
                    [self removeModule:ast];
                    return [JSNil new];
                }
            } else if ([xs count] == 2 && [JSKeyword isKeyword:[xs first]]) {
                ast = [[JSList alloc] initWithArray:[@[[[JSSymbol alloc] initWithArity:2 string:@"get" moduleName:[Const coreModuleName]],
                                                           [xs second], [xs first]] mutableCopy]];
                continue;
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
                //env = [[Env alloc] initWithEnv:[fn env] binds:[fn params] exprs:rest isImported:[fn isImported] currentEnv:env];
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

#pragma mark Module

/** Add given env to the modules table. */
- (void)addModule:(Env *)env {
    [Env setEnv:env forModuleName:[env moduleName]];
}

- (void)removeModule:(id<JSDataProtocol>)ast {
    JSSymbol *modSym = [self moduleNameFromAST:ast];
    [Env removeModule:[modSym value]];
    _env = _globalEnv;
    [self updateModuleName:[_env moduleName]];
}

/**
 (defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))
 (defmodule foo (export (greet 0)) (import (from bar (sum 1))))
 (defmodule foo (export (greet 0)) (export all) (import (from bar (sum 1))))
 */
- (JSSymbol *)defineModule:(id<JSDataProtocol>)ast {
    JSList *xs = (JSList *)ast;
    JSSymbol *modSym = [xs second];
    [modSym setIsModule:YES];
    NSString *modName = [modSym value];
    Env *modEnv = [[Env alloc] initWithModuleName:modName isUserDefined:YES];
    [self addModule:modEnv];
    [State setCurrentModuleName:[modEnv moduleName]];
    _env = modEnv; // change env to current module
    [self updateModuleName:modName];
    // The third element onwards are imports and exports
    [self processModuleDirectives:[xs drop:2] module:_env];
    if (_isREPL) _prompt = [modName stringByAppendingString:@"> "];
    return modSym;
}

- (NSInteger)arityFromObject:(id<JSDataProtocol>)object {
    id<JSDataProtocol> arityElem = object;
    NSInteger arity = -2;
    if ([JSSymbol isSymbol:object withName:@"n"]) {
        arity = -1;
    } else if ([JSNumber isNumber:arityElem]) {
        arity = [(JSNumber *)arityElem integerValue];
    }
    return arity;
}

- (void)processExportDirective:(JSList *)ast module:(Env *)env {
    JSList *elem = [ast rest];
    if ([elem count] == 1 && [JSSymbol isSymbol:[elem first] withName:@"all"]) {
        [env setIsExportAll:YES];
        return;
    } else {
        [env setIsExportAll:NO];
        NSMutableArray *fnList = [(JSList *)elem value];
        NSUInteger len = [fnList count];
        NSUInteger i = 0;
        for (i = 0; i < len; i++) {
            NSMutableArray *aExp = (NSMutableArray *)[(JSList *)fnList[i] value];
            JSSymbol *sym = (JSSymbol *)[aExp first];
            NSInteger arity = [self arityFromObject:[aExp second]];
            if (arity == -2) [[[JSError alloc] initWithDescription:ModuleArityDefinitionError] throw];
            [sym setArity:arity];
            [sym setInitialArity:arity];
            [sym updateArity];
            [sym setIsFault:YES];
            [sym setModuleName:[env moduleName]];
            [sym setInitialModuleName:[env moduleName]];
            [[env exportTable] setObject:[[JSFault alloc] initWithModule:[env moduleName] isImportFault:NO] forKey:sym];
        }
    }
}

- (void)processImportDirective:(JSList *)ast module:(Env *)env {
    JSList *imports = [ast rest];
    NSMutableArray *impArr = (NSMutableArray *)[imports value];
    NSUInteger len = [impArr count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        JSList *imp = (JSList *)impArr[i];
        if ([JSSymbol isSymbol:[imp first] withName:@"from"]) {
            NSString *modName = [(JSSymbol *)[imp second] value];
            Env *impEnv = [Env forModuleName:modName];
            if (impEnv) {
                NSMutableArray *impFns = (NSMutableArray *)[(JSList *)[imp drop:2] value];
                NSUInteger fnLen = [impFns count];
                NSUInteger j = 0;
                for (j = 0; j < fnLen; j++) {
                    JSList *aExp = (JSList *)impFns[j];
                    JSSymbol *sym = (JSSymbol *)[aExp first];
                    NSInteger arity = [self arityFromObject:[aExp second]];
                    if (arity == -2) [[[JSError alloc] initWithDescription:ModuleArityDefinitionError] throw];
                    [sym setArity:arity];
                    [sym setInitialArity:arity];
                    [sym updateArity];
                    [sym setIsFault:YES];
                    [sym setModuleName:[env moduleName]];
                    [sym setInitialModuleName:modName];
                    [sym setIsImported:YES];
                    [[env importTable] setObject:[[JSFault alloc] initWithModule:modName isImportFault:YES] forKey:sym];
                }
            }
        }
    }
}

/** Process module import exports. The ast can be of the form (export (a 1) (b 0)) (export (c 2) (d 1)) (import (from list (all 2)) (from io (read 1))) .. */
- (void)processModuleDirectives:(JSList *)ast module:(Env *)env {
    NSMutableArray *arr = [ast value];
    NSUInteger len = [arr count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        if ([JSList isList:arr[i]]) {
            JSList *xs = (JSList *)arr[i];
            id<JSDataProtocol> modDir = [xs first];
            if ([JSSymbol isSymbol:modDir withName:@"export"]) {
                [self processExportDirective:xs module:env];
            } else if ([JSSymbol isSymbol:modDir withName:@"import"]) {
                [self processImportDirective:xs module:env];
            }
        } else if ([JSString isString:arr[i]]) {
            [env setModuleDescription:[(JSString *)arr[i] value]];
        }
    }
}

- (JSSymbol *)moduleNameFromAST:(id<JSDataProtocol>)ast {
    JSList *xs = (JSList *)ast;
    JSList *modList = nil;
    if (![JSList isList:[xs second]]) [[[JSError alloc] initWithFormat:QuotedSymbol, @"module name"] throw];
    if ([modList count] > 2) [[[JSError alloc] initWithFormat:ArityError, 1, [modList count]] throw];
    modList = [xs second];
    return [modList second];
}

/** Change current module to the given one. */
- (JSSymbol *)changeModule:(id<JSDataProtocol>)ast {
    JSSymbol *modSym = [self moduleNameFromAST:ast];
    [modSym setIsModule:YES];
    NSString *modName = [modSym value];
    if ([modName isEqual:[Const defaultModuleName]]) {
        _env = _globalEnv;
    } else {
        // check modules table
        Env *modEnv = [Env forModuleName:modName];
        if (modEnv) {
            _env = modEnv;
        } else {
            [[self reader] setModuleName:[State currentModuleName]];
            [[[JSError alloc] initWithFormat:ModuleNotFound, modName] throw];
        }
    }
    [self updateModuleName:modName];
    return modSym;
}

/** Changes the prompt if in REPL and updates the @c currentModuleName */
- (void)updateModuleName:(NSString *)moduleName {
    if (_isREPL) _prompt = [moduleName stringByAppendingString:@"> "];
    [State setCurrentModuleName:moduleName];
    [[self reader] setModuleName:moduleName];
}

- (void)changeModuleTo:(NSString *)moduleName {
    Env *env = [Env forModuleName:moduleName];
    if (!env) [[[JSError alloc] initWithFormat:ModuleNotFound, moduleName] throw];
    [self setEnv:env];
    [State setCurrentModuleName:moduleName];
    [[self reader] setModuleName:moduleName];
    if (_isREPL) _prompt = [moduleName stringByAppendingString:@"> "];
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
        ret = [self print:[self eval:exps[i] withEnv:[self env]]];
    }
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
