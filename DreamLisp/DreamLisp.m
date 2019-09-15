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
    DLReader *_reader;
    DLPrinter *_printer;
    DLEnv *_globalEnv;
    /** Current env */
    DLEnv *_env;
    DLCore *_core;
    DLNetwork *_network;
    DLFileOps *_fileOps;
    DLIOService* _ioService;
    DLObjc *_objc;
    BOOL _isQuasiquoteMode;
    NSUInteger _quasiquoteDepth;
    NSUInteger _unquoteDepth;
    dispatch_queue_t _queue;
    dispatch_queue_t _repQueue;  // Used for REPL queue for processing symbols from symbol table
    BOOL _isREPL;  // Is running a REPL
    NSString *_prompt;
    BOOL _isDebug;
    BOOL _isVerbose;
}

@synthesize reader = _reader;
@synthesize globalEnv = _globalEnv;
@synthesize env = _env;
@synthesize isREPL = _isREPL;
@synthesize prompt = _prompt;
@synthesize isDebug = _isDebug;
@synthesize isVerbose = _isVerbose;

+ (void)initialize {
    hostLangVersion = [[NSString alloc] initWithFormat:@"%@ %.01f", @"Objective-C", (double)OBJC_API_VERSION];
    langVersion = [[NSString alloc] initWithFormat:@"DreamLisp v%@ [%@]", [DLConst dlVersion], hostLangVersion];
}

- (instancetype)initWithoutREPL {
    self = [super init];
    if (self) {
        _isREPL = NO;
        [self bootstrap];
        [self loadDLModuleLibs];
    }
    return self;
}

- (instancetype)init {
    self = [super init];
    return self;
}

- (void)bootstrap {
    _ioService = [DLIOService new];
    _fileOps = [DLFileOps new];
    [_ioService setFileIODelegate:_fileOps];
    [DLLogger setIOService:_ioService];
    _reader = [DLReader new];
    _printer = [DLPrinter new];
    _core = [DLCore new];
    [_core setDelegate:self];
    _objc = [DLObjc new];
    _network = [DLNetwork new];
    _env = [[DLEnv alloc] initWithModuleName:DLConst.defaultModuleName isUserDefined:NO];
    [_env setModuleDescription:[DLConst defaultModuleDescription]];
    [DLState setCurrentModuleName:[_env moduleName]];
    // Add modules to module table
    [self addModule:_env];  // default module
    [self addModule:[_core env]];  // core module
    [self addModule:[_network env]];
    _globalEnv = _env;
    _isQuasiquoteMode = NO;
    _quasiquoteDepth = 0;
    _queue = dispatch_queue_create("dl-dispatch-queue", nil);
    _repQueue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
    [self setLoadFileToREPL];
    [self setEvalToREPL];
    if (_isREPL) _prompt = [DLUtils promptWithModule:[DLState currentModuleName]];
}

- (void)setIsDebug:(BOOL)isDebug {
    _isDebug = isDebug;
    [DLLog setIsDebug:_isDebug];
}

- (void)setIsVerbose:(BOOL)isVerbose {
    _isVerbose = isVerbose;
    [DLLog setIsVerbose:_isVerbose];
}

#pragma mark Env setup

/** Add @c eval function to the environment. */
- (void)setEvalToREPL{
    DreamLisp * __weak weakSelf = self;
    id<DLDataProtocol>(^fn)(NSMutableArray *arg) = ^id<DLDataProtocol>(NSMutableArray *arg) {
        @autoreleasepool {
            DreamLisp *this = weakSelf;
            return [self eval:[arg first] withEnv:[this env]];
        }
    };
    NSString *coreModuleName = [DLConst coreModuleName];
    DLEnv *coreEnv = [DLEnv envForModuleName:coreModuleName];
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
        @autoreleasepool {
            DreamLisp *this = weakSelf;
            NSString *path = [[DLString dataToString:arg[0] fnName:@"load-file/1"] value];
            NSString *content = [this->_ioService readFile:path];
            if (!content) [[[DLError alloc] initWithFormat:DLFileNotFoundError, path] throw];
            BOOL hasError = NO;
            @try {
                [this rep:content];
            } @catch (NSException *exception) {
                hasError = YES;
                [this printException:exception log:YES readably:YES];
            }
            [this changeModuleTo:DLConst.defaultModuleName];
            return [[DLVector alloc] initWithArray:[@[[[DLKeyword alloc] initWithString: hasError ? @"fail" : @"ok"],
                                                      [[NSString alloc] initWithFormat:@"%@", [path lastPathComponent]]] mutableCopy]];
        }
    };
    DLEnv *coreEnv = [DLEnv envForModuleName:[DLConst coreModuleName]];
    DLSymbol *sym = [[DLSymbol alloc] initWithArity:1 string:@"load-file" moduleName:[DLConst coreModuleName]];
    DLFunction *fn = [[DLFunction alloc] initWithFn:loadFile argCount:1 name:@"load-file/1"];
    [fn setModuleName:[DLConst coreModuleName]];
    [[coreEnv exportTable] setObject:fn forKey:sym];
}

/** Construct built-in module file path. */
- (NSString *)moduleLibPath:(NSString *)path forModule:(NSString *)moduleName {
    return [[NSString alloc] initWithFormat:@"%@/%@.dlisp", path, moduleName];
}

/*! Load all DreamLisp modules libraries (which are written in dlisp itself). */
- (void)loadDLModuleLibs {
    NSString *moduleName;
    NSString *path;
    NSString *content;
    for (moduleName in DLConst.dlModuleLibs) {
        path = [self moduleLibPath:[_ioService resourcePath] forModule:moduleName];
        /* If the module is core, then we need to add the functions to the built-in code module. So we set the env to @c core. Else, we use the default @c user env. */
        _env = [moduleName isEqual:DLConst.coreModuleName] ? [_core env] : _globalEnv;
        [_reader setModuleName:moduleName];  /* set module name to the obtained module name */
        [self updateModuleName:moduleName];
        content = [_ioService readFile:path];
        if (!content) [[[DLError alloc] initWithFormat:DLModuleNotFound, moduleName] throw];
        @try {
            [self rep:content];
        } @catch (NSException *exception) {
            [self printException:exception log:YES readably:YES];
        }
        /* reset env and module name back to default */
        _env = _globalEnv;
        moduleName = [_env moduleName];
        [self updateModuleName:moduleName];
        [_reader setModuleName:moduleName];
    }
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
- (id<DLDataProtocol>)evalAST:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env {
    @autoreleasepool {
        if ([DLSymbol isSymbol:ast]) {
            return [env objectForKey:(DLSymbol *)ast];
        } else if ([DLList isList:ast]) {
            DLList *list = (DLList *)ast;
            NSMutableArray *arr = [NSMutableArray new];
            NSUInteger count = [list count];
            NSUInteger i = 0;
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
}

/** Checks if the given ast is either a non-empty list or vector. */
- (BOOL)isPair:(id<DLDataProtocol>)ast {
    return ([DLList isList:ast] && [(DLList *)ast count] > 0) || ([DLVector isVector:ast] && [(DLVector *)ast count] > 0);
}

/** Process quasiquote. */
- (id<DLDataProtocol>)quasiquote:(id<DLDataProtocol>)ast {
    @autoreleasepool {
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
}

/** Checks if the given ast is list with macro at function position. */
- (BOOL)isMacroCall:(id<DLDataProtocol>)ast env:(DLEnv *)env {
    //@autoreleasepool {
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
    //}
}

/** Expands a macro call. */
- (id<DLDataProtocol>)macroExpand:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env {
    NSMutableArray *xs = nil;
    DLFunction *fn;
    while ([self isMacroCall:ast env:env]) {
        xs = [(DLList *)ast value];
        fn = (DLFunction *)[env objectForKey:[[DLSymbol alloc] initWithArity:[xs count] - 1 symbol:[xs first]]];
        ast = [fn apply:[xs rest]];
    }
    return ast;
}

- (DLList *)toDoForm:(NSMutableArray *)arr {
    [arr insertObject:[[DLSymbol alloc] initWithName:@"do"] atIndex:0];
    return [[DLList alloc] initWithArray:arr];
}

/*!
 Parse the ast validating method expression form, giving a method object.

 (capitalizeString util-obj :lisp-case :max-length 10)
 (capitalizeString util-obj "olive" :case :lisp-case :max-length 10)
 (capitalizeString utils-obj (some-exp) (gen-keyword) (gen-val))
 (capitalizeString utils-obj)  ; Here there is no argument => the selector does not have a colon at the end.

 @return DLMethod
 */
- (DLMethod *)parseMethodForm:(id<DLDataProtocol>)ast object:(id<DLProxyProtocol>)object withEnv:(DLEnv *)env {
    DLMethod *method = [DLMethod new];
    method.params = [NSMutableArray new];
    if (![DLList isList:ast]) [[[DLError alloc] initWithDescription:DLMethodExpressionParseError] throw];
    DLList *xs = (DLList *)ast;
    id<DLDataProtocol> elem = [xs next];
    if (![DLSymbol isSymbol:elem]) [[[DLError alloc] initWithDescription:DLMethodExpressionParseError] throw];
    NSUInteger selCount = 0;  /* The second arg onwards */
    ++xs.seekIndex;  /* We have the object, so increment the position. */
    method.name = elem;
    id<DLDataProtocol> val = nil;
    id<DLDataProtocol> selKeyword = nil;
    DLMethodParam *methodParam = nil;
    while ([xs hasNext]) {
        elem = [xs next];
        methodParam = [DLMethodParam new];
        if (xs.seekIndex % 2 != 0) {  /* => value */
            if ([DLKeyword isKeyword:elem]) {  /* => enum */
                // TODO: convert keyword to NS enum
                methodParam.value = elem;
            } else {  /* we need to eval the form */
                val = [self eval:elem withEnv:env];
                if ([DLKeyword isKeyword:val]) {
                    // TODO: convert keyword to NS enum value
                    methodParam.value = val;
                } else {
                    methodParam.value = val;
                }
            }
        } else {  /* keyword, part of the selector */
            if (![DLKeyword isKeyword:elem]) {
                selKeyword = [self eval:elem withEnv:env];
                if (![DLKeyword isKeyword:selKeyword]) [[[DLError alloc] initWithDescription:DLMethodExpressionParseError] throw];
            } else {
                selKeyword = elem;
            }
            methodParam.selectorName = selKeyword;
            ++selCount;
        }
        [method.params addObject:methodParam];
    }
    method.env = env;
    [DLUtils updateSELForMethod:method];
    [DLUtils updateSelectorStringForMethod:method];
    if (method.params.count == 1 && selCount == 0) return method;
    if (selCount != [method.params count]) [[[DLError alloc] initWithFormat:DLMethodArityError, selCount, [method.params count]] throw];
    return method;
}

/** Evaluate the expression with the given environment. */
- (id<DLDataProtocol>)eval:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env {
    while (true) {
        @autoreleasepool {
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
                                DLEnv *catchEnv = [[DLEnv alloc] initWithEnv:env binds:[@[(DLSymbol *)[catchxs second]] mutableCopy]
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
                            @autoreleasepool {
                                DLEnv *fnEnv = [[DLEnv alloc] initWithEnv:env binds:[(DLList *)[xs second] value] exprs:arg];
                                return [self eval:form withEnv:fnEnv];
                            }
                        };
                        return [[DLFunction alloc] initWithAst:form params:[(DLList *)[xs second] value] env:env macro:NO meta:nil fn:fn];
                    } else if ([[sym value] isEqual:@"let"]) {
                        @autoreleasepool {
                            DLEnv *letEnv = [[DLEnv alloc] initWithEnv:env];
                            NSMutableArray *bindings = [DLVector isVector:[xs second]] ? [(DLVector *)[xs second] value] : [(DLList *)[xs second] value];
                            NSUInteger len = [bindings count];
                            NSUInteger i = 0;
                            id<DLDataProtocol> val = nil;
                            for (i = 0; i < len; i += 2) {
                                val = [self eval:[bindings nth: i + 1] withEnv:letEnv];
                                [letEnv setObject:val forKey:[DLSymbol symbolWithArityCheck:[bindings nth:i] withObject:val]];
                            }
                            ast = [self toDoForm:[xs drop:2]];
                            env = letEnv;
                            continue;
                        }
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
                    } else if ([[sym value] isEqual:@"defclass"]) {
                        DLClass *cls = [_objc parseClassForm:ast];
                        [_objc defclass:cls];
                        [self.env setObject:cls forKey:cls.name];
                        return cls;
                    } else if ([[sym value] isEqual:@"make-instance"]) {
                        DLInvocation *invocation = [_objc parseMakeInstanceForm:ast withEnv:env];
                        if (!invocation) [[[DLError alloc] initWithFormat:DLRTObjectInitError, invocation.cls.className] throw];
                        return [_objc makeInstance:invocation];  /* Returns a DLObject */
                    } else if ([[sym value] isEqual:@"defmethod"]) {
                        DLMethod *method = [_objc parseMethod:ast withEnv:env];
                        method.env = env;
                        method.ast = [self toDoForm:[@[method.ast] mutableCopy]];  // TODO: add method to class
                        [_objc addMethodToClass:method];
                        method.name.arity = [[method params] count];
                        [method.name updateArity];
                        DLSymbol *methodSym = [[DLSymbol alloc] initWithName:method.selectorString.value];
                        methodSym.moduleName = [_env moduleName];
                        methodSym.initialModuleName = methodSym.moduleName;
                        methodSym.initialArity = method.name.arity;
                        [methodSym resetArity];
                        [env setObject:method forKey:methodSym];
                        return method;
                    }
                } else if ([xs count] == 2 && [DLKeyword isKeyword:[xs first]]) {
                    ast = [[DLList alloc] initWithArray:[@[[[DLSymbol alloc] initWithArity:2 string:@"get" moduleName:[DLConst coreModuleName]],
                                                           [xs first], [xs second]] mutableCopy]];
                    continue;
                }
                /** Could be a function or a method. */
                // NB: since we fetch the function from the env, all built-in functions should work with the object type - either working directly with the
                // proxy object or forwarding the invocation
                NSMutableArray *list = nil;
                @try {
                    list = [(DLList *)[self evalAST:ast withEnv:env] value];
                } @catch (NSException *excep) {  /* => The given symbol does not match any existing function. Check if the second arg is an object. */
                    if ([DLList isList:ast]) {
                        DLList *aList = (DLList *)ast;
                        DLSymbol *objSym = [aList second];
                        DLSymbol *aSym = (DLSymbol *)[aList first];
                        if (![DLSymbol isSymbol:objSym]) {
                            aSym.initialArity = aList.count - 1;
                            [aSym updateArity];
                            @throw [DLError exceptionWithFormat:DLSymbolNotFound, [aSym string]];
                        }
                        id<DLDataProtocol> obj = [env objectForKey:objSym isThrow:NO];
                        if (obj && [DLObject isObject:obj]) {  // TODO: usecase: if the obj is a class
                            id<DLProxyProtocol> dlObj = (DLObject *)obj;
                            DLMethod *method = [self parseMethodForm:aList object:dlObj withEnv:env];
                            DLSymbol *methodSym = [[DLSymbol alloc] initWithName:method.selectorString.value];
                            methodSym.moduleName = objSym.moduleName;
                            methodSym.initialModuleName = objSym.initialModuleName;
                            methodSym.initialArity = method.params.count;
                            [methodSym resetArity];
                            DLMethod *theMethod = [env objectForKey:methodSym isThrow:NO];
                            if (theMethod) {  /* User defined method. Since the method body is s-exp we need to eval the body. */
                                [DLLog debug:@"DLMethod found in env"];
                                NSMutableArray *args = [NSMutableArray new];
                                [args addObject:self];
                                [args addObject:theMethod];
                                [args addObject:[method args]];
                                [_objc invokeMethodWithReturn:method.selector withObject:dlObj args:args];
                                return [dlObj returnValue];
                            }
                            /* Method not defined within dlisp, assuming built-in. */
                            [_objc invokeMethodWithReturn:method.selector withObject:dlObj args:[method args]];
                            [DLLog debugWithFormat:@"object ret: %@", [dlObj returnValue]];
                            return [dlObj returnValue];
                        }
                    }
                    @throw excep;
                }
                id<DLDataProtocol> elem = [list first];
                if ([DLFunction isFunction:elem]) {
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
                        env = [[DLEnv alloc] initWithEnv:[fn env] binds:[fn params] exprs:rest];
                    } else {
                        return [fn apply:rest];
                    }
                    continue;
                } else {
                    [[[DLError alloc] initWithFormat:DLSymbolNotFound, elem] throw];
                }
            } else if ([DLHashMap isHashMap:ast]) {
                DLHashMap *dict = (DLHashMap *)ast;
                NSArray *keys = [dict allKeys];
                NSUInteger i = 0;
                NSUInteger len = [keys count];
                DLHashMap *ret = [DLHashMap new];
                id key = nil;
                id<DLDataProtocol> val = nil;
                id<DLDataProtocol> object = nil;
                for (i = 0; i < len; i++) {
                    key = keys[i];
                    val = [dict objectForKey:key];
                    object = [self eval:val withEnv:env];
                    [ret setObject:object forKey:[self eval:key withEnv:env]];
                }
                return ret;
            } else {
                return [self evalAST:ast withEnv:env];
            }
        }
    }
}

#pragma mark Module

/** Add given env to the modules table. */
- (void)addModule:(DLEnv *)env {
    [DLEnv setEnv:env forModuleName:[env moduleName]];
}

- (void)removeModule:(id<DLDataProtocol>)ast {
    NSString *modName = (NSString *)[[self moduleNameFromAST:ast] value];
    [DLEnv removeModule:modName];
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
    DLEnv *modEnv = [[DLEnv alloc] initWithModuleName:modName isUserDefined:YES];
    [self addModule:modEnv];
    [DLState setCurrentModuleName:[modEnv moduleName]];
    _env = modEnv; // change env to current module
    [self updateModuleName:modName];
    // The third element onwards are imports and exports
    [self processModuleDirectives:[xs drop:2] module:_env];
    if (_isREPL) _prompt = [DLUtils promptWithModule:modName];
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

- (void)processExportDirective:(DLList *)ast module:(DLEnv *)env {
    DLList *elem = [ast rest];
    if ([elem count] == 1 && [DLSymbol isSymbol:[elem first] withName:@"all"]) {
        [env setIsExportAll:YES];
        return;
    } else {
        [env setIsExportAll:NO];
        NSMutableArray *fnList = [(DLList *)elem value];
        NSUInteger len = [fnList count];
        NSUInteger i = 0;
        NSMutableArray *aExp = nil;
        DLSymbol *sym = nil;
        NSInteger arity = -2;
        for (i = 0; i < len; i++) {
            aExp = (NSMutableArray *)[(DLList *)fnList[i] value];
            sym = (DLSymbol *)[aExp first];
            arity = [self arityFromObject:[aExp second]];
            if (arity == -2) [[[DLError alloc] initWithDescription:DLModuleArityDefinitionError] throw];
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

- (void)processImportDirective:(DLList *)ast module:(DLEnv *)env {
    DLList *imports = [ast rest];
    NSMutableArray *impArr = (NSMutableArray *)[imports value];
    NSUInteger len = [impArr count];
    NSUInteger i = 0;
    DLList *imp = nil;
    NSString *modName = nil;
    DLEnv *impEnv = nil;
    NSMutableArray *impFns;
    NSUInteger fnLen;
    NSUInteger j = 0;
    DLList *aExp;
    DLSymbol *sym;
    NSInteger arity;
    for (i = 0; i < len; i++) {
        imp = (DLList *)impArr[i];
        if ([DLSymbol isSymbol:[imp first] withName:@"from"]) {
            modName = [(DLSymbol *)[imp second] value];
            impEnv = [DLEnv envForModuleName:modName];
            if (impEnv) {
                impFns = (NSMutableArray *)[(DLList *)[imp drop:2] value];
                fnLen = [impFns count];
                for (j = 0; j < fnLen; j++) {
                    aExp = (DLList *)impFns[j];
                    sym = (DLSymbol *)[aExp first];
                    arity = [self arityFromObject:[aExp second]];
                    if (arity == -2) [[[DLError alloc] initWithDescription:DLModuleArityDefinitionError] throw];
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
- (void)processModuleDirectives:(DLList *)ast module:(DLEnv *)env {
    NSMutableArray *arr = [ast value];
    NSUInteger len = [arr count];
    NSUInteger i = 0;
    DLList *xs;
    id<DLDataProtocol> modDir;
    for (i = 0; i < len; i++) {
        if ([DLList isList:arr[i]]) {
            xs = (DLList *)arr[i];
            modDir = [xs first];
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
    if ([xs count] > 2) [[[DLError alloc] initWithFormat:DLArityError, 1, [xs count]] throw];
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
    if ([modName isEqual:DLConst.defaultModuleName]) {
        _env = _globalEnv;
    } else {
        // check modules table
        DLEnv *modEnv = [DLEnv envForModuleName:modName];
        if (modEnv) {
            _env = modEnv;
        } else {
            [[self reader] setModuleName:[DLState currentModuleName]];
            [[[DLError alloc] initWithFormat:DLModuleNotFound, modName] throw];
        }
    }
    [self updateModuleName:modName];
    return modStr;
}

/** Changes the prompt if in REPL and updates the @c currentModuleName */
- (void)updateModuleName:(NSString *)moduleName {
    if (_isREPL) _prompt = [DLUtils promptWithModule:moduleName];
    [DLState setCurrentModuleName:moduleName];
    [[self reader] setModuleName:moduleName];
}

- (void)changeModuleTo:(NSString *)moduleName {
    DLEnv *env = [DLEnv envForModuleName:moduleName];
    if (!env) {
        [[[DLError alloc] initWithFormat:DLModuleNotFound, moduleName] throw];
        return;
    }
    [self setEnv:env];
    [DLState setCurrentModuleName:moduleName];
    [[self reader] setModuleName:moduleName];
    if (_isREPL) _prompt = [DLUtils promptWithModule:moduleName];
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
        @autoreleasepool {
            ret = [self print:[self eval:exps[i] withEnv:[self env]]];
        }
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
            [DLLog error:desc];
        } else if ([[info allKeys] containsObject:@"dldata"]) {
            id<DLDataProtocol> data = (id<DLDataProtocol>)[info valueForKey:@"dldata"];
            if (data) {
                desc = [[NSString alloc] initWithFormat:@"Error: %@", [_printer printStringFor:data readably:readably]];
                if (desc && log) [DLLog error:desc];
            }
        } else if ([[info allKeys] containsObject:@"NSUnderlyingError"]) {
            NSError *err = [info valueForKey:@"NSUnderlyingError"];
            [DLLog error:[err localizedDescription]];
        }
    } else {
        desc = exception.description;
        if (desc && log) [DLLog error:desc];
    }
    return desc;
}

#pragma mark Delegate

- (nonnull id<DLDataProtocol>)eval:(nonnull id<DLDataProtocol>)ast {
    @autoreleasepool {
        return [self eval:ast withEnv:[self env]];
    }
}

- (DLIOService *)ioService {
    return _ioService;
}

@end

