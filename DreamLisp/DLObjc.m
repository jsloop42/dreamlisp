//
//  DLObjc.m
//  DreamLisp
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLObjc.h"

static NSString *_description = @"Module that interfaces with Objective-C Runtime.";
static NSString *_moduleName = @"objcrt";

/*! A class that interfaces with the DLObjcRT providing a more higher level DL methods. */
@implementation DLObjc {
    DLEnv *_env;
    DLObjcRT *_rt;
    DLObjcMethodAttrKey *_methodAttrKey;
}

- (void)dealloc {
    [DLLog debug:@"DLObjc dealloc"];
    [super dealloc];
}

- (instancetype)init {
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _env = [[[DLEnv alloc] init] autorelease];
    [_env setModuleName:_moduleName];
    [_env setModuleDescription:_description];
    [_env setIsUserDefined:NO];
    _rt = [[DLObjcRT new] autorelease];
    _methodAttrKey = DLObjcMethodAttrKey.shared;
}

- (BOOL)addDLDataProtocol:(Class)cls {
    return [_rt addDLDataProtocol:cls proto:@"DLDataProtocol"];
}

/*! Checks if the given status is @c true, else throws an exception with the given message. */
- (void)checkError:(BOOL)status msg:(NSString *)msg {
    if (!status) [[[[DLError alloc] initWithDescription:msg] autorelease] throw];
}

- (void)addProperty:(DLClass *)cls {
    if (cls.slots.count > 0) {
        DLSlot *slot = nil;
        for (slot in cls.slots) {
            [_rt addProperty:cls.value name:slot.value.value attr:slot.attribute];
        }
    }
}

/*!
 Checks whether the given class name is registered with the runtime.

 @return BOOL
 */
- (BOOL)isClassExists:(NSString *)className {
    return [_rt lookUpClass:className] != nil;
}

/*! Registers the proxy class with the Objective-C runtime. */
- (void)defclass:(DLClass *)cls {
    DLSymbol *supClassSym = cls.conformance.count >= 1 ? cls.conformance.firstObject : nil;
    Class aCls = nil;
    NSString *proxyClassName = cls.name.value;
    if ([self isClassExists:proxyClassName]) @throw [DLError exceptionWithFormat:DLClassExistsError, proxyClassName];
    if (supClassSym) {
        aCls = [_rt allocateClass:proxyClassName superclass:NSClassFromString(supClassSym.value)];
    } else {
        aCls = [_rt allocateClass:proxyClassName superclass:nil];
    }
    cls.proxy = aCls;
    [self addProperty:cls];  /* Add property */
    BOOL ret = [_rt addMethod:aCls name:@selector(dataType) imp:[_rt implementationFor:[cls class] selector:@selector(dataType)]
                         type:[[[[NSString alloc] initWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] autorelease] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    ret = [_rt addMethod:aCls name:@selector(dataTypeName) imp:[_rt implementationFor:[cls class] selector:@selector(dataTypeName)]
                    type:[[[[NSString alloc] initWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] autorelease] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    ret = [_rt addMethod:aCls name:@selector(meta) imp:[_rt implementationFor:[cls class] selector:@selector(meta)]
                    type:[[[[NSString alloc] initWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] autorelease] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    ret = [_rt addMethod:aCls name:@selector(moduleName) imp:[_rt implementationFor:[cls class] selector:@selector(moduleName)]
                    type:[[[[NSString alloc] initWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] autorelease] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    /* Add init methods */
    DLSlot *slot;
    for (slot in cls.slots) {
        [_rt addMethod:aCls name:[slot selectorForInitArg] imp:(IMP)dl_initWithPropImp type:slot.methodType];
        // todo:
        /* Add getter and setter methods */
        
    }
    [_rt registerClass:aCls];
    [self addDLDataProtocol:aCls];
    [self checkError:ret msg:DLRTConformProtocolError];
}

/**
 Parses the the defclass ast and returns a @c DLClass object.
 Sample expression: (defclass person (NSObject NSCopying) ((name :initarg :with-name) (age :non-atomic :type :NSString)))
 */
- (DLClass *)parseClassForm:(id<DLDataProtocol>)ast {
    NSString *fnName = @"defclass";
    DLList *list = [DLList dataToList:ast fnName:fnName];  /* Here we are not checking if the first element is defclass because it's already done before */
    if (![DLSymbol isSymbol:[list next] withName:fnName]) {
        [[[[DLError alloc] initWithFormat:DLSymbolMismatchError, @"'defclass'", [[list first] dataTypeName]] autorelease] throw];
    }
    id<DLDataProtocol> elem = [list next];
    DLClass *cls = [[DLClass new] autorelease];
    if (elem) {  // class name sym
        cls.name = [DLSymbol dataToSymbol:elem position:list.seekIndex fnName:fnName];
        /* Add class, delegate details if present */
        elem = [list next];
        if (elem) {
            DLList *cnf = [DLList dataToList:elem fnName:fnName];
            if (![cnf isEmpty]) {
                id<DLDataProtocol> aSym = nil;
                NSMutableArray *cnfArr = cnf.value;
                NSUInteger len = cnfArr.count;
                NSUInteger i = 0;
                for (i = 0; i < len; i++) {
                    aSym = [cnfArr objectAtIndex:i];
                    if (![DLSymbol isSymbol:aSym]) [[[[DLError alloc] initWithFormat:DLClassConformanceParseError, i, [aSym dataTypeName]] autorelease] throw];
                    [cls.conformance addObject:aSym];
                }
            }
        }
        /* Add slot info if present */
        elem = [list next];
        if (elem) {
            DLList *slotsList = [DLList dataToList:elem fnName:fnName];  // The outer slot container
            if ([slotsList count] > 0) {  // There are individual slots
                id<DLDataProtocol> slotData = nil;
                DLSlot *slot = nil;
                NSMutableArray *slotsArr = slotsList.value;
                DLList *slotList = nil;
                for (slotData in slotsArr) {  // Process each slot
                    slotList= [DLList dataToList:slotData fnName:fnName];  // Not all elements in the slot list is mandatory.
                    slot = [DLSlot new];
                    DLObjcPropertyAttr *attr = [DLObjcPropertyAttr new];
                    id<DLDataProtocol> token = nil;
                    DLKeyword *kwd = nil;
                    while ([slotList hasNext]) {  // Process each element of a slot
                        if (slotList.seekIndex > 0) {
                            token = [slotList next];  // This increments the seek index
                            if ([DLKeyword isKeyword:token]) {
                                kwd = (DLKeyword *)token;
                                if ([[kwd value] isEqual:@":initarg"]) {
                                    DLKeyword *arg = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                    DLKeyword *initKwd = [[DLKeyword alloc] initWithString:[[[NSString alloc] initWithFormat:@"init-%@",
                                                                                             [arg string]] autorelease]];
                                    slot.initializationArg = initKwd;
                                    [initKwd release];
                                    slot.methodType = [[[[NSString alloc] initWithFormat:@"%s%s%s%s%s", @encode(id), @encode(id), @encode(SEL), @encode(id),
                                                        @encode(id)] autorelease] UTF8String];  // id:return id:self, SEL, prop, DLClass*
                                } else if ([[kwd value] isEqual:@":read-only"]) {
                                    attr.isReadOnly = YES;
                                } else if ([[kwd value] isEqual:@":read-write"]) {
                                    attr.isReadOnly = NO;
                                } else if ([[kwd value] isEqual:@":non-atomic"]) {
                                    attr.isNonAtomic = YES;
                                } else if ([[kwd value] isEqual:@":atomic"]) {
                                    attr.isNonAtomic = NO;
                                } else if ([[kwd value] isEqual:@":copy"]) {
                                    attr.isCopy = YES;
                                } else if ([[kwd value] isEqual:@":getter"]) {  /* Specifies a custom getter */
                                    attr.hasCustomGetter = YES;
                                    attr.getter = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                } else if ([[kwd value] isEqual:@":setter"]) {  /* Specifies a custom setter */
                                    attr.hasCustomSetter = YES;
                                    attr.setter = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                } else if ([[kwd value] isEqual:@":dynamic"]) {
                                    attr.isDynamic = YES;
                                } else if ([[kwd value] isEqual:@":weak"]) {
                                    attr.isWeakReference = YES;
                                } else if ([[kwd value] isEqual:@":type"]) {
                                    DLKeyword *typeKwd = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                    NSString *typeStr = [typeKwd string];
                                    slot.propertyType = [_rt typeFromKeywordString:typeStr];
                                    attr.type = [[[[NSString alloc] initWithFormat:@"T%s", slot.propertyType] autorelease] UTF8String];
                                    if (strncmp(attr.type, "T@", sizeof(attr.type)) && ![typeStr isEqual:@"any"]) {  // => The type is an object and has more specific than an id.
                                        attr.type = [[[[NSString alloc] initWithFormat:@"%s\"%@\"", attr.type, typeStr] autorelease] UTF8String];
                                    }
                                } else {
                                    [[[[DLError alloc] initWithFormat:DLRTSlotFormatError, kwd] autorelease] throw];
                                }
                            }
                        } else {
                            token = [slotList next];
                            slot.value = [DLSymbol dataToSymbol:token position:0 fnName:fnName];  // First element is the slot name, which is a symbol
                            attr.value = slot.value.value;
                        }
                    }
                    [DLUtils updatePropertyAttr:attr];
                    slot.attribute = attr;
                    [attr release];
                    [cls.slots addObject:slot];
                    [slot release];
                }
            }
        }
    }
    return cls;
}

/*! Invoke a selector on the given object */
- (id<DLDataProtocol>)invokeMethod:(SEL)selector withObject:(id<DLProxyProtocol>)object args:(NSMutableArray *)args {
    DLInvocation *invo = [_rt invocationForMethod:selector withProxy:object args:args];
    return [_rt invoke:invo.invocation];
}

/*! Invokes the given selector on the proxy object with args and sets the return value if any in the object. */
- (void)invokeMethodWithReturn:(SEL)selector withObject:(id<DLProxyProtocol>)object args:(NSMutableArray *)args {
    id ret = [self invokeMethod:selector withObject:object args:args];
    [ret retain];  /* Use use assign in the property. So this object needs to be retained so that the property points to a valid memory location. */
    id<DLDataProtocol> elem = [DLUtils convertFromFoundationTypeToDLType:ret];
    [object setReturnValue:elem];
}

- (void)addMethodToClass:(DLMethod *)method {
    method.imp = (IMP)dl_methodImp;
    [_rt addMethod:method.cls.proxy name:[method selector] imp:method.imp type:method.type];
}

- (void)addMethodParamAttrs:(DLMethodParam *)param fromMeta:(DLList *)meta fnName:(NSString *)fnName {
    DLKeyword *kwd = nil;
    while ([meta hasNext]) {  /* The meta list with type and other attrs */
        DLList *list = [DLList dataToList:[meta next] fnName:fnName];
        param.attr = [[DLObjcMethodAttr new] autorelease];
        while ([list hasNext]) {
            kwd = [list next];
            if ([kwd isEqual:_methodAttrKey.kNullable]) {
                param.attr.isNullable = YES;
            } else {  /* Assuming this to be a type */
                param.attr.type = kwd;  // TODO: validate type info
            }
        }
    }
}

/*!
 Parses the given ast into a DLMethod

 (defmethod ^[:class] gen-random 'utils (n :max max)  (..method-body..))  ; class method
 (defmethod gen-random 'utils (n :max max)  (..method-body..))  ; instance method
 (defmethod ^[:ns-decimal] gen-random 'utils (^[:ns-uinteger] n ^[:ns-integer :nullable] :max max)  (..method-body..))  ; instance method gen-random/2
 (defmethod ^[:NSNumber :nullable] gen-random 'utils (^[:ns-uinteger] n)  (..method-body..))  ; instance method that takes only one arg gen-random/1
 */
- (DLMethod *)parseMethod:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env {
    NSString *fnName = @"defmethod";
    DLList *xs = [DLList dataToList:ast fnName:fnName];
    NSUInteger len;
    ++xs.seekIndex;  /* The first element is the symbol defmethod. So increment the index. */
    if (![xs hasNext]) [[[[DLError alloc] initWithFormat:DLMethodNameNotFoundError, fnName] autorelease] throw];
    id<DLDataProtocol> elem = [xs next];
    DLMethod *method = [[DLMethod new] autorelease];
    method.params = [[NSMutableArray new] autorelease];
    /* Get meta info if present */
    if ([DLList isList:elem]) {
        DLList *meta = (DLList *)elem;
        if ([meta isEmpty]) [[[[DLError alloc] initWithFormat:DLMethodNameNotFoundError, fnName] autorelease] throw];
        if ([DLSymbol isSymbol:[meta next] withName:@"with-meta"]) {  /* Meta encountered */
            id<DLDataProtocol>metaElem = [meta next];
            method.name = [DLSymbol dataToSymbol:metaElem position:meta.seekIndex - 1 fnName:fnName];
            DLMethodParam *param = [[DLMethodParam new] autorelease];
            [self addMethodParamAttrs:param fromMeta:meta fnName:fnName];
            method.attr = param.attr;
        }
    } else if ([DLSymbol isSymbol:elem]) {  /* No return meta, add method name */
        method.name = [DLSymbol dataToSymbol:elem position:xs.seekIndex - 1 fnName:fnName];
    }
    method.name.value = [DLUtils lispCaseToCamelCase:method.name.value];
    /* Add method's class */
    if (![xs hasNext]) [[[[DLError alloc] initWithFormat:DLMethodClassNotSpecifiedError, fnName] autorelease] throw];
    elem = [DLList dataToList:[xs next] fnName:fnName];
    DLClass *cls = [self classInfoFromAST:elem fnName:fnName env:env];
    method.cls = cls;
    /* Add method params */
    DLList *argList = [DLList dataToList:[xs next] position:xs.seekIndex - 1 fnName:fnName];
    len = [argList count];
    DLList *meta = nil;
    DLMethodParam *param = nil;
    BOOL wasPreviousAKeyword = NO;
    if (len > 0) {
        id<DLDataProtocol> metaElem = nil;
        while ([argList hasNext]) {
            elem = [argList next];
            if ([DLList isList:elem]) {
                meta = (DLList *)elem;
                if ([DLSymbol isSymbol:[meta next] withName:@"with-meta"]) {  /* Meta encountered */
                    param = [[DLMethodParam new] autorelease];
                    param.position = argList.seekIndex - 1;
                    metaElem = [meta next];
                    if ([DLSymbol isSymbol:metaElem]) {  /* arg name */
                        param.name = metaElem;
                    } else if ([DLKeyword isKeyword:metaElem]) {  /* The selector part */
                        param.selectorName = metaElem;
                    }
                    [self addMethodParamAttrs:param fromMeta:meta fnName:fnName];
                    [method.params addObject:param];
                    param = nil;
                    wasPreviousAKeyword = NO;
                }
            } else if ([DLKeyword isKeyword:elem]) {  /* An arg selector without a meta */
                if ([method.params count] == 0) {  /* Keyword cannot appear at the first position */
                    [[[[DLError alloc] initWithFormat:DLMethodParseError, @"'meta' or 'symbol'", 0, [elem dataTypeName]] autorelease] throw];
                }
                param = [[DLMethodParam new] autorelease];
                param.position = argList.seekIndex - 1;
                param.selectorName = elem;
                wasPreviousAKeyword = YES;
            } else if ([DLSymbol isSymbol:elem]) {
                if (param && wasPreviousAKeyword) {  /* => Previous a selector found */
                    param.name = elem;
                } else {  /* Must be the first argument without a meta */
                    param = [[DLMethodParam new] autorelease];
                    param.name = elem;
                    param.position = 0;
                }
                [method.params addObject:param];
                param = nil;
                wasPreviousAKeyword = NO;
            }
        }
    }
    /* Method body */
    if (![xs hasNext]) [[[[DLError alloc] initWithDescription:DLMethodBodyNotFoundError] autorelease] throw];
    method.ast = [xs next];
    [DLUtils updateSELForMethod:method];
    [DLUtils updateSelectorStringForMethod:method];
    [cls.methods addObject:method];
    return method;
}

- (NSString *)generateAssocKey:(NSUInteger)hash {
    return [[[NSString alloc] initWithFormat:@"dlproxy_%ld_%ld", [DLState.shared assocObjectCounter], hash] autorelease];
}

- (DLObject *)objectWithProxy:(id)proxy withClass:(DLClass *)cls {
    DLObject *object = [[[DLObject alloc] initWithProxy:proxy] autorelease];
    object.cls = cls;
    return object;
}

- (DLObject * _Nullable)makeInstance:(DLInvocation *)invocation {
    id inst = [_rt invoke:invocation.invocation];
    [inst retain];
    if (!inst) {
        [[[[DLError alloc] initWithFormat:DLRTObjectInitError, [invocation.invocation.target className]] autorelease] throw];
        return nil;
    }
    return [self objectWithProxy:inst withClass:invocation.cls];
}

- (DLClass * _Nullable)classInfoFromAST:(DLList *)list fnName:(NSString *)fnName env:(DLEnv *)env {
    if ([list count] != 2) [[[[DLError alloc] initWithDescription:DLClassNameParseError] autorelease] throw];
    id<DLDataProtocol> quoteElem = [list next];
    if (![DLSymbol isSymbol:quoteElem withName:@"quote"]) {
        [[[[DLError alloc] initWithFormat:DLDataTypeMismatchWithArity, @"quote", list.seekIndex - 1, [quoteElem dataTypeName]] autorelease] throw];
    }
    DLSymbol *clsSym = [list next];
    id<DLDataProtocol> elem = [env objectForKey:clsSym isThrow:NO];  /* Get the class from the env */
    if (elem) return [DLClass dataToClass:elem fnName:fnName];
    /* DLClass not found, check if the class is loaded into the RT */
    Class clazz = [_rt lookUpClass:clsSym.value];
    if (!clazz) {
        [[[[DLError alloc] initWithFormat:DLClassNotFoundError, [clsSym value]] autorelease] throw];
        return nil;
    }
    return [[[DLClass alloc] initWithProxy:clazz] autorelease];
}

/*!
 Parses the make-instance form giving an invocation object which can be used to create an object.

 (make-instance 'person :init-with-name "Olive")
 (make-instance 'series :init-with-count 3.141)
 (make-instance 'person :alloc)
 (make-instance 'NSString :init-with-c-string "olive" :encoding :ns-utf8-string)
 */
- (DLInvocation  * _Nullable)parseMakeInstanceForm:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env {
    NSString *fnName = @"make-instance";
    DLList *list = [DLList dataToList:ast fnName:fnName];
    NSUInteger len = [list count];
    if (len < 2) [[[[DLError alloc] initWithFormat:DLMakeInstanceNoneSpecifiedError, @"'class'"] autorelease] throw];
    ++list.seekIndex;  /* The first element is the symbol make-instance. So increment the index. */
    /* Get the class info */
    DLClass *cls = [self classInfoFromAST:[list next] fnName:fnName env:env];
    id<DLDataProtocol> elem = nil;
    BOOL isAllocEncountered = NO;
    BOOL isInitEncountered = NO; /* Any form of init. Either :init or the one specified in :initarg. */
    NSMutableArray *selKeywordArr = [[NSMutableArray new] autorelease];
    NSMutableArray *argsArr = [[NSMutableArray new] autorelease];
    DLKeyword *kwd = nil;
    DLInvocation *invocation = nil;
    while ([list hasNext]) {
        elem = [list next];
        if ([DLKeyword isKeyword:elem]) {
            kwd = (DLKeyword *)elem;
            NSString *kwdName = [kwd string];
            if ([kwdName isEqual:@"alloc"]) {
                isAllocEncountered = YES;
            } else if ([kwdName isEqual:@"init"]) {
                isInitEncountered = YES;
            } else {
                DLSlot *slot = [cls slotWithInitArg:kwd];
                if (slot) {   /* initarg encountered which means, initializing with property value */
                    /*
                     (defclass person (nsobject) ((name :initarg :with-name)))
                     (defmethod init-with-name ((obj person) name :with-age age) (.. body))
                                ^__method name              ^__arg   ^__second paramerterized arg, followed by the arg variable, age
                     This will get converted into
                       By convention all methods returns some value, if none given, use id (NSNull).
                       Since methods starting with init expects to return instancetype, we add the return type as instancetype
                     - (instancetype)initWithName:(id)name withAge:(id)age;

                     (defmethod add-work (obj person) (..))
                     (make-instance 'person :with-name "Olive")
                     (make-instance 'person :init-with-name "Olive" :with-age 32)
                     */
                    SEL sel = [DLTypeUtils convertInitKeywordToSelector:[kwd string]];
                    if (![list hasNext]) [[[[DLError alloc] initWithFormat:DLMakeInstanceNoneSpecifiedError, @"initarg value"] autorelease] throw];
                    id<DLDataProtocol>arg = [list next];  /* the param value */
                    invocation = [_rt invocationFromClass:cls forSelector:sel args:[@[arg, cls] mutableCopy]];
                    return invocation;
                }

            }
        }
        /* Not a slot based init */
        if (!isAllocEncountered && !isInitEncountered) {
            while ([list hasNext]) {
                if (list.seekIndex % 2 != 0) {
                    [selKeywordArr addObject:[DLKeyword dataToKeyword:elem fnName:fnName]];
                } else {
                    [argsArr addObject:elem];
                }
                elem = [list next];
            }
            [argsArr addObject:elem];
            if ([selKeywordArr isEmpty]) [[[[DLError alloc] initWithDescription:DLMakeInstanceNoSELFoundError] autorelease] throw];
            if ([selKeywordArr count] != [argsArr count]) {
                [[[[DLError alloc] initWithFormat:DLMakeInstanceSELArgCountMismatchError, [selKeywordArr count], [argsArr count]] autorelease] throw];
            }
            //[argsArr addObject:cls];
            SEL sel = [DLUtils convertKeywordToSelector:selKeywordArr];
            invocation = [_rt invocationFromClass:cls forSelector:sel args:argsArr];
            return invocation;
        }
    }
    return nil;
}

@end
