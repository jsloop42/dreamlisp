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
    [DLLog debug:[NSString stringWithFormat:@"%@ dealloc", [self className]]];
}

- (instancetype)init {
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _env = [[DLEnv alloc] init];
    [_env setModuleName:_moduleName];
    [_env setModuleDescription:_description];
    [_env setIsUserDefined:NO];
    _rt = [DLObjcRT new];
    _methodAttrKey = DLObjcMethodAttrKey.shared;
}

- (BOOL)addDLDataProtocol:(Class)cls {
    return [_rt addDLDataProtocol:cls proto:@"DLDataProtocol"];
}

- (void)checkError:(BOOL)status msg:(NSString *)msg {
    if (!status) [[[DLError alloc] initWithDescription:msg] throw];
}

- (void)addProperty:(DLClass *)cls {
    if (cls.slots.count > 0) {
        DLSlot *slot = nil;
        for (slot in cls.slots) {
            [_rt addProperty:cls.value name:slot.value.value attr:slot.attribute];
        }
    }
}

- (void)defclass:(DLClass *)cls {
    DLSymbol *supClassSym = cls.conformance.count >= 1 ? cls.conformance.firstObject : nil;
    Class aCls = nil;
    if (supClassSym) {
        aCls = [_rt allocateClass:cls.name.value superclass:NSClassFromString(supClassSym.value)];
    } else {
        aCls = [_rt allocateClass:cls.name.value superclass:nil];
    }
    cls.value = aCls;
    [self addProperty:cls];  /* Add property */
    BOOL ret = [_rt addMethod:aCls name:@selector(dataType) imp:[_rt implementationFor:[cls class] selector:@selector(dataType)]
                         type:[[NSString stringWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    ret = [_rt addMethod:aCls name:@selector(dataTypeName) imp:[_rt implementationFor:[cls class] selector:@selector(dataTypeName)]
                    type:[[NSString stringWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    ret = [_rt addMethod:aCls name:@selector(meta) imp:[_rt implementationFor:[cls class] selector:@selector(meta)]
                    type:[[NSString stringWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    ret = [_rt addMethod:aCls name:@selector(moduleName) imp:[_rt implementationFor:[cls class] selector:@selector(moduleName)]
                    type:[[NSString stringWithFormat:@"%s%s%s", @encode(id), @encode(id), @encode(SEL)] UTF8String]];
    [self checkError:ret msg:DLRTAddMethodError];
    /* Add init methods */
    DLSlot *slot;
    for (slot in cls.slots) {
        [_rt addMethod:aCls name:[slot selectorForInitArg] imp:(IMP)dl_initWithPropImp type:slot.methodType];
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
        [[[DLError alloc] initWithFormat:DLSymbolMismatchError, @"'defclass'", [[list first] dataTypeName]] throw];
    }
    id<DLDataProtocol> elem = [list next];
    DLClass *cls = nil;
    if (elem) {  // class name sym
        cls = [DLClass new];
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
                    if (![DLSymbol isSymbol:aSym]) [[[DLError alloc] initWithFormat:DLClassConformanceParseError, i, [aSym dataTypeName]] throw];
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
                for (slotData in slotsArr) {  // Process each slot
                    DLList *slotList = [DLList dataToList:slotData fnName:fnName];  // Not all elements in the slot list is mandatory.
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
                                    DLKeyword *initKwd = [[DLKeyword alloc] initWithString:[NSString stringWithFormat:@"init-%@", [arg string]]];
                                    slot.initializationArg = initKwd;
                                    const char *type = [[NSString stringWithFormat:@"%s%s%s%s%s", @encode(id), @encode(id), @encode(SEL), @encode(id), @encode(id)] UTF8String];  // id:return id:self, SEL, prop, DLClass*
                                    slot.methodType = type;
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
                                    attr.customGetter = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                } else if ([[kwd value] isEqual:@":setter"]) {  /* Specifies a custom setter */
                                    attr.hasCustomSetter = YES;
                                    attr.customSetter = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                } else if ([[kwd value] isEqual:@":dynamic"]) {
                                    attr.isDynamic = YES;
                                } else if ([[kwd value] isEqual:@":weak"]) {
                                    attr.isWeakReference = YES;
                                } else if ([[kwd value] isEqual:@":type"]) {
                                    DLKeyword *typeKwd = [DLKeyword dataToKeyword:[slotList next] position:slotList.seekIndex - 1 fnName:fnName];
                                    NSString *typeStr = [typeKwd string];
                                    slot.propertyType = [_rt typeFromKeywordString:typeStr];
                                    attr.type = [[NSString stringWithFormat:@"T%s", slot.propertyType] UTF8String];
                                    if (strncmp(attr.type, "T@", sizeof(attr.type)) && ![typeStr isEqual:@"any"]) {  // => The type is an object and has more specific than an id.
                                        attr.type = [[NSString stringWithFormat:@"%s\"%@\"", attr.type, typeStr] UTF8String];
                                    }
                                } else {
                                    [[[DLError alloc] initWithFormat:DLRTSlotFormatError, kwd] throw];
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
                    [cls.slots addObject:slot];
                }
            }
        }
    }
    return cls;
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
    NSUInteger len = [xs count];
    ++xs.seekIndex;  /* The first element is the symbol defmethod. So increment the index. */
    /* Get meta info if present */
    NSUInteger i = 0;
    NSUInteger count = 0;
    id<DLDataProtocol> elem = [xs next];
    DLMethod *method = [DLMethod new];
    method.params = [NSMutableArray new];
    DLMethodParam *params = [DLMethodParam new];
    DLObjcMethodAttr *methodAttr = [DLObjcMethodAttr new];
    if ([DLList isKindOfList:elem]) { /* meta details found */
        DLVector *metaxs = (DLVector *)elem;
        DLKeyword *kwd = nil;
        len = [metaxs count];
        for (i = 0; i < count; i++) {
            if (i > 0) {
                kwd = [metaxs nth:i];
                if ([kwd isEqual:_methodAttrKey.kNullable]) {
                    methodAttr.isNullable = YES;
                } else { /* If none of the methor arr key matches => this is taken as a type */
                    methodAttr.type = kwd;  //TODO: validate the type
                }
            }
        }
        method.attr = methodAttr; /* Set the method' return attr details */
    }
    /* Add method name */
    if (![xs hasNext]) [[[DLError alloc] initWithFormat:DLMethodNameNotFoundError, fnName] throw];
    elem = [xs next];
    method.name = [DLSymbol dataToSymbol:elem position:xs.seekIndex - 1 fnName:fnName];
    /* Add method's class */
    if (![xs hasNext]) [[[DLError alloc] initWithFormat:DLMethodClassNotSpecifiedError, fnName] throw];
    elem = [DLList dataToList:[xs next] fnName:fnName];
    method.cls = [self classInfoFromAST:xs fnName:fnName env:env];
    /* Add method params */
    // TODO: method params
    return method;
}


- (DLObject *)makeInstance:(DLInvocation *)invocation {
    id inst = [_rt instantiateFromInvocation:invocation.invocation];
    if (!inst) [[[DLError alloc] initWithFormat:DLRTObjectInitError, [invocation.invocation.target className]] throw];
    DLObject *object = [[DLObject alloc] initWithObject:inst];
    object.cls = invocation.cls;
    invocation.object = object;
    return object;
}

- (DLClass *)classInfoFromAST:(DLList *)list fnName:(NSString *)fnName env:(DLEnv *)env {
    DLList *clsList = [DLList dataToList:[list next] position:list.seekIndex - 1 fnName:fnName];
    id<DLDataProtocol> quoteElem = [clsList next];
    if (![DLSymbol isSymbol:quoteElem withName:@"quote"]) {
        [[[DLError alloc] initWithFormat:DLDataTypeMismatchWithArity, @"quote", clsList.seekIndex - 1, [quoteElem dataTypeName]] throw];
    }
    DLSymbol *clsSym = [clsList next];
    return [env objectForKey:clsSym isThrow:YES];  /* Get the class from the env */
}

/*!
 Parses the make-instance form giving an invocation object which can be used to create an object.

 (make-instance 'person :init-with-name "Olive")
 (make-instance 'series :init-with-count 3.141)
 (make-instance 'person :alloc)
 */
- (DLInvocation *)parseMakeInstanceForm:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env {
    NSString *fnName = @"make-instance";
    DLList *list = [DLList dataToList:ast fnName:fnName];
    NSUInteger len = [list count];
    if (len < 2) [[[DLError alloc] initWithFormat:DLMakeInstanceNoneSpecifiedError, @"'class'"] throw];
    ++list.seekIndex;  /* The first element is the symbol make-instance. So increment the index. */
    /* Get the class info */
    DLClass *cls = [self classInfoFromAST:list fnName:fnName env:env];
    id<DLDataProtocol> elem = nil;
    BOOL isAllocEncountered = NO;
    BOOL isInitEncountered = NO; /* Any form of init. Either :init or the one specified in :initarg. */
    while ([list hasNext]) {
        elem = [list next];
        if ([DLKeyword isKeyword:elem]) {
            DLKeyword *kwd = (DLKeyword *)elem;
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
                    if (![list hasNext]) [[[DLError alloc] initWithFormat:DLMakeInstanceNoneSpecifiedError, @"initarg value"] throw];
                    id<DLDataProtocol>arg = [list next];
                    DLInvocation *invocation = [_rt invocationFromClass:cls forSelector:sel args:[@[arg, cls] mutableCopy]];
                    return invocation;
                }
            }
            // TODO: case where we init using a defined method instead of a property
            //} else if (keyword starts with init) {
            // get values in the form: id :keyword id :keyword id
            //}
        }
    }
    return nil;
}

@end
