//
//  DreamLisp.m
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DreamLisp.h"

static NSString *coreLibFileName = @"core.dlisp";
static NSString *hostLangVersion;
static NSString *langVersion;

@implementation DreamLisp {
    Reader *_reader;
    Printer *_printer;
    Env *_globalEnv;
    /** Current env */
    Env *_env;
    Core *_core;
    IOService* _ioService;
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
    langVersion = [[NSString alloc] initWithFormat:@"DreamLisp v%@ [%@]", [Const dlVersion], hostLangVersion];
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
    _ioService = [IOService new];
    [_ioService setFileIODelegate:[FileOps new]];
    [Logger setIOService:_ioService];
    _reader = [Reader new];
    _printer = [Printer new];
    _core = [Core new];
    [_core setDelegate:self];
    _env = [[Env alloc] initWithModuleName:[Const defaultModuleName] isUserDefined:NO];
    [_env setModuleDescription:[Const defaultModuleDescription]];
    [State setCurrentModuleName:[_env moduleName]];
    // Add modules to module table
    [self addModule:_env];  // default module
    [self addModule:[_core env]];  // core module
    _globalEnv = _env;
    _isQuasiquoteMode = NO;
    _quasiquoteDepth = 0;
    _queue = dispatch_queue_create("dl-dispatch-queue", nil);
    _repQueue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
    [self setLoadFileToREPL];
    [self setEvalToREPL];
    if (_isREPL) _prompt = [[State currentModuleName] stringByAppendingString:@"> "];
}

#pragma mark Env setup

/** Add @c eval function to the environment. */
- (void)setEvalToREPL{
    DreamLisp * __weak weakSelf = self;
    id<DLDataProtocol>(^fn)(NSMutableArray *arg) = ^id<DLDataProtocol>(NSMutableArray *arg) {
        DreamLisp *this = weakSelf;
        return [self eval:[arg first] withEnv:[this env]];
    };
    NSString *coreModuleName = [Const coreModuleName];
    Env *coreEnv = [Env forModuleName:coreModuleName];
    [coreEnv setObject:[[DLFunction alloc] initWithFn:fn argCount:1 name:@"eval/1"]
                forKey:[[DLSymbol alloc] initWithArity:1 string:@"eval" moduleName:coreModuleName]];
    [coreEnv setObject:[DLList new] forKey:[[DLSymbol alloc] initWithName:@"*ARGV*" moduleName:coreModuleName]];
    [coreEnv setObject:[[DLString alloc] initWithFormat:@"%@", hostLangVersion]
                forKey:[[DLSymbol alloc] initWithName:@"*host-language*" moduleName:coreModuleName]];
    [coreEnv setObject:[[DLString alloc] initWithFormat:@"%@", langVersion] forKey:[[DLSymbol alloc] initWithName:@"*version*" moduleName:coreModuleName]];
    [coreEnv setObject:[DLList new] forKey:[[DLSymbol alloc] initWithName:@"*ARGV*" moduleName:coreModuleName]];
}

/** Add @c load-file to the environment, which loads and evaluates the expressions contained in the file. */
- (void)setLoadFileToREPL {
    DreamLisp * __weak weakSelf = self;
    id<DLDataProtocol>(^loadFile)(NSMutableArray *arg) = ^id<DLDataProtocol>(NSMutableArray *arg) {
        DreamLisp *this = weakSelf;
        NSString *path = [[DLString dataToString:arg[0] fnName:@"load-file/1"] value];
        NSString *content = [this->_ioService readFile:path];
        BOOL hasError = NO;
        @try {
            [this rep:content];
        } @catch (NSException *exception) {
            hasError = YES;
            [this printException:exception log:YES readably:YES];
        }
        [this changeModuleTo:[Const defaultModuleName]];
        return [[DLVector alloc] initWithArray:[@[[[DLKeyword alloc] initWithString: hasError ? @"fail" : @"ok"],
                                                  [[NSString alloc] initWithFormat:@"%@", [path lastPathComponent]]] mutableCopy]];
    };
    Env *coreEnv = [Env forModuleName:[Const coreModuleName]];
    DLSymbol *sym = [[DLSymbol alloc] initWithArity:1 string:@"load-file" moduleName:[Const coreModuleName]];
    DLFunction *fn = [[DLFunction alloc] initWithFn:loadFile argCount:1 name:@"load-file/1"];
    [fn setModuleName:[Const coreModuleName]];
    [[coreEnv exportTable] setObject:fn forKey:sym];
}

/** Construct core lib file path. */
- (NSString *)coreLibPath:(NSString *)path {
    return [[NSString alloc] initWithFormat:@"%@/%@", path, coreLibFileName];
}

/** Load core lib @c core.dlisp from the framework's resource path. */
- (void)loadCoreLib {
    NSString *path = [self coreLibPath:[_ioService resourcePath]];
    NSString *moduleName = [Const coreModuleName];
    _env = [_core env];
    [_reader setModuleName:moduleName];
    [self updateModuleName:moduleName];
    NSString *content = [_ioService readFile:path];
    @try {
        [self rep:content];
    } @catch (NSException *exception) {
        [self printException:exception log:YES readably:YES];
    }
    _env = _globalEnv;
    moduleName = [_env moduleName];
    [self updateModuleName:moduleName];
    [_reader setModuleName:moduleName];
}

- (void)printVersion {
    if (_isREPL) [_ioService writeOutput:langVersion];
}

#pragma mark Read

/** Converts the given string expression to AST */
- (NSMutableArray<id<DLDataProtocol>> *)read:(NSString *)string {
    return [_reader readString:string];
}

#pragma mark Eval

/** Evaluate the AST with the given environment. */
- (id<DLDataProtocol>)evalAST:(id<DLDataProtocol>)ast withEnv:(Env *)env {
    if ([DLSymbol isSymbol:ast]) {
        return [env objectForKey:(DLSymbol *)ast];
    } else if ([DLList isList:ast]) {
        DLList *list = (DLList *)ast;
        NSUInteger count = [list count];
        NSUInteger i = 0;
        NSMutableArray *arr = [NSMutableArray new];
        if ([DLSymbol isSymbol:[list first]]) {
            // Get the function correspoding to the symbol
            DLSymbol *sym = (DLSymbol *)[list first];
            [arr addObject:[self eval:[[DLSymbol alloc] initWithArity:count - 1 position:[sym position] symbol:sym] withEnv:env]];
            i = 1;
        }
        for(; i < count; i = i + 1) {
            [arr addObject:[self eval:[list nth:i] withEnv:env]];
        }
        return [[DLList alloc] initWithArray:arr];
    } if ([DLVector isVector:ast]) {
        NSMutableArray *arr = [(DLVector *)ast map: ^id<DLDataProtocol>(id<DLDataProtocol> xs) {
            return [self eval:xs withEnv:env];
        }];
        return [[DLVector alloc] initWithArray:arr];
    } if ([DLHashMap isHashMap:ast]) {
        NSMapTable *table = [(DLHashMap *)ast value];
        NSUInteger i = 0;
        NSArray *keys = [table allKeys];
        NSUInteger len = [keys count];
        for (i = 0; i < len; i++) {
            [table setObject:[self eval:[table objectForKey:keys[i]] withEnv:env] forKey:keys[i]];
        }
        return [[DLHashMap alloc] initWithMapTable:table];
    }
    return ast;
}

/** Checks if the given ast is either a non-empty list or vector. */
- (BOOL)isPair:(id<DLDataProtocol>)ast {
    return ([DLList isList:ast] && [(DLList *)ast count] > 0) || ([DLVector isVector:ast] && [(DLVector *)ast count] > 0);
}

/** Process quasiquote. */
- (id<DLDataProtocol>)quasiquote:(id<DLDataProtocol>)ast {
    if (![self isPair:ast]) {
        return [[DLList alloc] initWithArray:[@[[[DLSymbol alloc] initWithName:@"quote"], ast] mutableCopy]];
    }
    DLList *lst = (DLList *)ast;
    NSMutableArray *xs = [lst value];
    id<DLDataProtocol> first = [xs first];
    if ([DLSymbol isSymbol:first] && [[(DLSymbol *)first value] isEqual:@"unquote"]) return [xs second];
    if ([self isPair:first]) {
        if ([DLVector isVector:first] && [xs count] == 1) {
            NSMutableArray *arr = [NSMutableArray new];
            [arr addObject:[[DLSymbol alloc] initWithName:@"vector"]];
            [arr addObjectsFromArray:[(DLVector *)first value]];
            // (quote (vector 1 2 3)) -> DLList:(quote DLList:(vector 1 2 3))
            return [[DLList alloc] initWithArray:[@[[[DLSymbol alloc] initWithName:@"list"], [[DLList alloc] initWithArray:arr]] mutableCopy]];
        }
        NSMutableArray *list = [(DLList *)first value];
        if (![list isEmpty] && [DLSymbol isSymbol:[list first]] && [[(DLSymbol *)[list first] value] isEqual:@"splice-unquote"]) {
            return [[DLList alloc] initWithArray:[@[[[DLSymbol alloc] initWithName:@"concat"], [list second],
                                                    [self quasiquote:[[DLList alloc] initWithArray:[xs rest]]]] mutableCopy]];
        }
    }
    return [[DLList alloc] initWithArray:[@[[[DLSymbol alloc] initWithName:@"cons"], [self quasiquote:first],
                                            [self quasiquote:[[DLList alloc] initWithArray:[xs rest]]]] mutableCopy]];

}

/** Checks if the given ast is list with macro at function position. */
- (BOOL)isMacroCall:(id<DLDataProtocol>)ast env:(Env *)env {
    if ([DLList isList:ast]) {
        NSMutableArray *xs = [(DLList *)ast value];
        id<DLDataProtocol> first = [xs first];
        if (first && [DLSymbol isSymbol:first]) {
            DLSymbol *sym = [[DLSymbol alloc] initWithArity:[xs count] - 1 position:0 symbol:first];
            id<DLDataProtocol> fnData = [env objectForKey:sym isThrow:NO];
            if (fnData && [DLFunction isFunction:fnData]) return [(DLFunction *)fnData isMacro];
        }
    }
    return NO;
}

/** Expands a macro call. */
- (id<DLDataProtocol>)macroExpand:(id<DLDataProtocol>)ast withEnv:(Env *)env {
    while ([self isMacroCall:ast env:env]) {
        NSMutableArray *xs = [(DLList *)ast value];
        DLFunction *fn = (DLFunction *)[env objectForKey:[[DLSymbol alloc] initWithArity:[xs count] - 1 symbol:[xs first]]];
        ast = [fn apply:[xs rest]];
    }
    return ast;
}

- (DLList *)toDoForm:(NSMutableArray *)arr {
    [arr insertObject:[[DLSymbol alloc] initWithName:@"do"] atIndex:0];
    return [[DLList alloc] initWithArray:arr];
}

/** Evaluate the expression with the given environment. */
- (id<DLDataProtocol>)eval:(id<DLDataProtocol>)ast withEnv:(Env *)env {
    while (true) {
        if ([DLVector isVector:ast]) {
            NSMutableArray *xs = [(DLVector *)ast map:^id<DLDataProtocol>(id<DLDataProtocol> obj) {
                return [self eval:obj withEnv:env];
            }];
            return [[DLVector alloc] initWithArray:xs];
        } else if ([DLList isList:ast]) {
            ast = [self macroExpand:ast withEnv:env];
            if (![DLList isList:ast]) return [self evalAST:ast withEnv:env];
            NSMutableArray *xs = [(DLList *)ast value];
            if ([xs isEmpty]) return ast;
            if ([DLSymbol isSymbol:[xs first]]) {
                // special forms
                DLSymbol *sym = (DLSymbol *)[xs first];
                if ([[sym value] isEqual:@"def"]) {
                    id<DLDataProtocol> val = [self eval:[xs nth:2] withEnv:env];
                    DLSymbol *sym = [DLSymbol symbolWithArityCheck:[xs second] withObject:val];
                    [sym copyMeta:val];
                    [env setObject:val forKey:sym];
                    return val;
                } else if ([[sym value] isEqual:@"defmacro"]) {
                    NSMutableArray *args = [NSMutableArray new];
                    BOOL isWithMeta = NO;
                    DLSymbol *fnSym = [[DLSymbol alloc] initWithName:@"fn"];
                    DLSymbol *bind = [DLSymbol dataToSymbol:[xs second] fnName:@"defmacro"];
                    if ([DLList isList:[xs nth:3]]) {  // check for with-meta
                        DLList *list = (DLList *)[xs nth:3];
                        if ([DLSymbol isSymbol:[list first] withName:@"with-meta"]) {
                            isWithMeta = YES;
                            [args addObject:[list first]];  // with-meta sym
                            NSMutableArray *fnArr = [NSMutableArray new];
                            [fnArr addObject:fnSym];  // fn sym
                            [fnArr addObject:[xs nth:2]];  // fn arg
                            [fnArr addObject:[list second]];  // fn body
                            [args addObject:[[DLList alloc] initWithArray:fnArr]];  // with-meta fn list
                            [args addObject:[list nth:2]];  // meta value
                        } else {
                            args = [xs drop:2];
                            [args add:fnSym atIndex:0];
                        }
                    } else {
                        args = [xs drop:2];
                        [args add:fnSym atIndex:0];
                    }
                    DLList *fnList = [[DLList alloc] initWithArray: args];
                    DLFunction *fn = (DLFunction *)[self eval:fnList withEnv:env];
                    [fn setName:[(DLSymbol *)[xs second] value]];
                    DLFunction *macro = [[DLFunction alloc] initWithMacro:fn];
                    if ([macro hasMeta]) [bind setMeta:[macro meta]];
                    [env setObject:macro forKey:[DLSymbol symbolWithArityCheck:bind withObject:macro]];
                    return macro;
                } else if ([[sym value] isEqual:@"try"]) {
                    NSMutableArray *form = [xs rest];
                    @try {
                        if ([DLList isList:[xs last]]) {
                            DLList *last = [xs last];
                            if ([DLSymbol isSymbol:[last first] withName:@"catch"]) {
                                form = [form dropLast];
                            }
                        }
                        return [self eval:[self toDoForm:form] withEnv:env];
                    } @catch (NSException *exception) {
                        if ([xs count] > 2) {
                            DLList *catchxs = (DLList *)[xs nth:2];
                            if ([DLSymbol isSymbol:[catchxs first]] && [[(DLSymbol *)[catchxs first] value] isNotEqualTo:@"catch"]) {
                                [[[DLError alloc] initWithData:[catchxs first]] throw];
                            }
                            Env *catchEnv = [[Env alloc] initWithEnv:env binds:[@[(DLSymbol *)[catchxs second]] mutableCopy]
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
                    id<DLDataProtocol> last = [xs last];
                    ast = last ? last : [DLNil new];
                    continue;
                } else if ([[sym value] isEqual:@"if"]) {
                    id<DLDataProtocol> res = [self eval:[xs second] withEnv:env];
                    if ([DLNil isNil:res] || ([DLBool isBool:res] && [(DLBool *)res value] == NO)) {
                        ast = [xs count] > 3 ? [xs nth:3] : [DLNil new];
                    } else {
                        ast = [xs nth:2];
                    }
                    continue;
                } else if ([[sym value] isEqual:@"fn"]) {
                    DLList *form = (DLList *)[self toDoForm:[xs drop:2]];
                    id<DLDataProtocol>(^fn)(NSMutableArray *) = ^id<DLDataProtocol>(NSMutableArray * arg) {
                        Env *fnEnv = [[Env alloc] initWithEnv:env binds:[(DLList *)[xs second] value] exprs:arg];
                        return [self eval:form withEnv:fnEnv];
                    };
                    return [[DLFunction alloc] initWithAst:form params:[(DLList *)[xs second] value] env:env macro:NO meta:nil fn:fn];
                } else if ([[sym value] isEqual:@"let"]) {
                    Env *letEnv = [[Env alloc] initWithEnv:env];
                    NSMutableArray *bindings = [DLVector isVector:[xs second]] ? [(DLVector *)[xs second] value] : [(DLList *)[xs second] value];
                    NSUInteger len = [bindings count];
                    NSUInteger i = 0;
                    for (i = 0; i < len; i += 2) {
                        id<DLDataProtocol> val = [self eval:[bindings nth: i + 1] withEnv:letEnv];
                        [letEnv setObject:val forKey:[DLSymbol symbolWithArityCheck:[bindings nth:i] withObject:val]];
                    }
                    ast = [self toDoForm:[xs drop:2]];
                    env = letEnv;
                    continue;
                } else if ([[sym value] isEqual:@"quote"]) {
                    return [xs second];
                } else if ([[sym value] isEqual:@"quasiquote"]) {
                    id<DLDataProtocol> exp = [xs second];
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
                    return [DLNil new];
                }
            } else if ([xs count] == 2 && [DLKeyword isKeyword:[xs first]]) {
                ast = [[DLList alloc] initWithArray:[@[[[DLSymbol alloc] initWithArity:2 string:@"get" moduleName:[Const coreModuleName]],
                                                       [xs first], [xs second]] mutableCopy]];
                continue;
            }
            // Function
            NSMutableArray *list = [(DLList *)[self evalAST:ast withEnv:env] value];
            DLFunction *fn = [DLFunction dataToFunction:[list first]];
            // The symbol in the ast at first position is resolved to corrsponding function in the list, which is in `fn` variable.
            // NB: Any value properties can be update from the corresponding symbol at this position.
            DLSymbol *bind = (DLSymbol *)[(DLList *)ast first];
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
        } else if ([DLHashMap isHashMap:ast]) {
            DLHashMap *dict = (DLHashMap *)ast;
            NSArray *keys = [dict allKeys];
            NSUInteger i = 0;
            NSUInteger len = [keys count];
            DLHashMap *ret = [DLHashMap new];
            for (i = 0; i < len; i++) {
                id key = keys[i];
                id<DLDataProtocol> val = (id<DLDataProtocol>)[dict objectForKey:key];
                id<DLDataProtocol> object = (id<DLDataProtocol>)[self eval:val withEnv:env];
                [ret setObject:object forKey:[self eval:(id<DLDataProtocol>)key withEnv:env]];
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

- (void)removeModule:(id<DLDataProtocol>)ast {
    NSString *modName = (NSString *)[[self moduleNameFromAST:ast] value];
    [Env removeModule:modName];
    _env = _globalEnv;
    [self updateModuleName:[_env moduleName]];
}

/**
 (defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))
 (defmodule foo (export (greet 0)) (import (from bar (sum 1))))
 (defmodule foo (export (greet 0)) (export all) (import (from bar (sum 1))))
 */
- (DLSymbol *)defineModule:(id<DLDataProtocol>)ast {
    DLList *xs = (DLList *)ast;
    DLSymbol *modSym = [DLSymbol dataToSymbol:[xs second] position:1 fnName:@"defmodule/3"];
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

- (NSInteger)arityFromObject:(id<DLDataProtocol>)object {
    id<DLDataProtocol> arityElem = object;
    NSInteger arity = -2;
    if ([DLSymbol isSymbol:object withName:@"n"]) {
        arity = -1;
    } else if ([DLNumber isNumber:arityElem]) {
        arity = [(DLNumber *)arityElem integerValue];
    }
    return arity;
}

- (void)processExportDirective:(DLList *)ast module:(Env *)env {
    DLList *elem = [ast rest];
    if ([elem count] == 1 && [DLSymbol isSymbol:[elem first] withName:@"all"]) {
        [env setIsExportAll:YES];
        return;
    } else {
        [env setIsExportAll:NO];
        NSMutableArray *fnList = [(DLList *)elem value];
        NSUInteger len = [fnList count];
        NSUInteger i = 0;
        for (i = 0; i < len; i++) {
            NSMutableArray *aExp = (NSMutableArray *)[(DLList *)fnList[i] value];
            DLSymbol *sym = (DLSymbol *)[aExp first];
            NSInteger arity = [self arityFromObject:[aExp second]];
            if (arity == -2) [[[DLError alloc] initWithDescription:ModuleArityDefinitionError] throw];
            [sym setArity:arity];
            [sym setInitialArity:arity];
            [sym updateArity];
            [sym setIsFault:YES];
            [sym setModuleName:[env moduleName]];
            [sym setInitialModuleName:[env moduleName]];
            [[env exportTable] setObject:[[DLFault alloc] initWithModule:[env moduleName] isImportFault:NO] forKey:sym];
        }
    }
}

- (void)processImportDirective:(DLList *)ast module:(Env *)env {
    DLList *imports = [ast rest];
    NSMutableArray *impArr = (NSMutableArray *)[imports value];
    NSUInteger len = [impArr count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        DLList *imp = (DLList *)impArr[i];
        if ([DLSymbol isSymbol:[imp first] withName:@"from"]) {
            NSString *modName = [(DLSymbol *)[imp second] value];
            Env *impEnv = [Env forModuleName:modName];
            if (impEnv) {
                NSMutableArray *impFns = (NSMutableArray *)[(DLList *)[imp drop:2] value];
                NSUInteger fnLen = [impFns count];
                NSUInteger j = 0;
                for (j = 0; j < fnLen; j++) {
                    DLList *aExp = (DLList *)impFns[j];
                    DLSymbol *sym = (DLSymbol *)[aExp first];
                    NSInteger arity = [self arityFromObject:[aExp second]];
                    if (arity == -2) [[[DLError alloc] initWithDescription:ModuleArityDefinitionError] throw];
                    [sym setArity:arity];
                    [sym setInitialArity:arity];
                    [sym updateArity];
                    [sym setIsFault:YES];
                    [sym setModuleName:[env moduleName]];
                    [sym setInitialModuleName:modName];
                    [sym setIsImported:YES];
                    [[env importTable] setObject:[[DLFault alloc] initWithModule:modName isImportFault:YES] forKey:sym];
                }
            }
        }
    }
}

/** Process module import exports. The ast can be of the form (export (a 1) (b 0)) (export (c 2) (d 1)) (import (from list (all 2)) (from io (read 1))) .. */
- (void)processModuleDirectives:(DLList *)ast module:(Env *)env {
    NSMutableArray *arr = [ast value];
    NSUInteger len = [arr count];
    NSUInteger i = 0;
    for (i = 0; i < len; i++) {
        if ([DLList isList:arr[i]]) {
            DLList *xs = (DLList *)arr[i];
            id<DLDataProtocol> modDir = [xs first];
            if ([DLSymbol isSymbol:modDir withName:@"export"]) {
                [self processExportDirective:xs module:env];
            } else if ([DLSymbol isSymbol:modDir withName:@"import"]) {
                [self processImportDirective:xs module:env];
            }
        } else if ([DLString isString:arr[i]]) {
            [env setModuleDescription:[(DLString *)arr[i] value]];
        }
    }
}

- (DLString *)moduleNameFromAST:(id<DLDataProtocol>)ast {
    DLList *xs = (DLList *)ast;
    if ([xs count] > 2) [[[DLError alloc] initWithFormat:ArityError, 1, [xs count]] throw];
    DLString *modName = nil;
    if ([DLString isString:[xs second]]) {
        modName = (DLString *)[xs second];
    } else {
        modName = [DLString dataToString:[self eval:[xs second] withEnv:[self env]] fnName:@"in-module/1"];
    }
    return modName;
}

/** Change current module to the given one. */
- (DLString *)changeModule:(id<DLDataProtocol>)ast {
    DLString *modStr = [self moduleNameFromAST:ast];
    NSString *modName = (NSString *)[modStr value];
    if ([modName isEqual:[Const defaultModuleName]]) {
        _env = _globalEnv;
    } else {
        // check modules table
        Env *modEnv = [Env forModuleName:modName];
        if (modEnv) {
            _env = modEnv;
        } else {
            [[self reader] setModuleName:[State currentModuleName]];
            [[[DLError alloc] initWithFormat:ModuleNotFound, modName] throw];
        }
    }
    [self updateModuleName:modName];
    return modStr;
}

/** Changes the prompt if in REPL and updates the @c currentModuleName */
- (void)updateModuleName:(NSString *)moduleName {
    if (_isREPL) _prompt = [moduleName stringByAppendingString:@"> "];
    [State setCurrentModuleName:moduleName];
    [[self reader] setModuleName:moduleName];
}

- (void)changeModuleTo:(NSString *)moduleName {
    Env *env = [Env forModuleName:moduleName];
    if (!env) {
        [[[DLError alloc] initWithFormat:ModuleNotFound, moduleName] throw];
        return;
    }
    [self setEnv:env];
    [State setCurrentModuleName:moduleName];
    [[self reader] setModuleName:moduleName];
    if (_isREPL) _prompt = [moduleName stringByAppendingString:@"> "];
}

#pragma mark Print

- (NSString *)print:(id<DLDataProtocol>)data {
    return [_printer printStringFor:data readably:YES];
}

#pragma mark REPL

/** Read-eval-print function. */
- (NSString * _Nullable)rep:(NSString *)string {
    NSMutableArray<id<DLDataProtocol>> *exps = [self read:string];
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
- (nullable id<DLDataProtocol>)exceptionInfo:(NSException *)exception {
    NSDictionary *info = exception.userInfo;
    if (info) {
        id<DLDataProtocol> data = (id<DLDataProtocol>)[info objectForKey:@"dldata"];
        if (data) return data;
        NSString *desc = [info valueForKey:@"description"];
        if (desc) return [[DLString alloc] initWithString:desc];
    }
    return ([exception.description isNotEmpty]) ? [[DLString alloc] initWithString:exception.description] : nil;
}

/** Prints the given exception details to stdout. */
- (NSString * _Nullable)printException:(NSException *)exception log:(BOOL)log readably:(BOOL)readably {
    NSString *desc = nil;
    if (exception.userInfo != nil) {
        NSDictionary *info = exception.userInfo;
        desc = [info valueForKey:@"description"];
        if (desc && log) {
            [Log error:desc];
        } else if ([[info allKeys] containsObject:@"dldata"]) {
            id<DLDataProtocol> data = (id<DLDataProtocol>)[info valueForKey:@"dldata"];
            if (data) {
                desc = [[NSString alloc] initWithFormat:@"Error: %@", [_printer printStringFor:data readably:readably]];
                if (desc && log) [Log error:desc];
            }
        } else if ([[info allKeys] containsObject:@"NSUnderlyingError"]) {
            NSError *err = [info valueForKey:@"NSUnderlyingError"];
            [Log error:[err localizedDescription]];
        }
    } else {
        desc = exception.description;
        if (desc && log) [Log error:desc];
    }
    return desc;
}

#pragma mark Delegate

- (nonnull id<DLDataProtocol>)eval:(nonnull id<DLDataProtocol>)ast {
    return [self eval:ast withEnv:[self env]];
}

- (IOService *)ioService {
    return _ioService;
}

@end

