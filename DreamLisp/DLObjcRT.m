//
//  DLObjcRT.m
//  DreamLisp
//
//  Created by jsloop on 30/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLObjcRT.h"

static NSRegularExpression *_setterPttn;

#pragma mark C Helpers

const char *dl_setterName(NSString *name) {
    NSArray *matches = [DLUtils matchesInString:name withExpression:_setterPttn];
    NSTextCheckingResult *match = [matches firstObject];
    if ([match numberOfRanges] == 4) {
        NSString *mstr = [name substringWithRange:[match rangeAtIndex:2]];
        return [[DLUtils toAccessorVarFromGetter:mstr] UTF8String];
    }
    return "";
}

void dl_setIvar(id self, SEL _cmd, id val) {
    NSString *cmdStr = NSStringFromSelector(_cmd);
    const char *setterVar = dl_setterName(cmdStr);
    Ivar ivar = class_getInstanceVariable([self class], setterVar);
    id oldVer = object_getIvar(self, ivar);
    if (oldVer != val) {
        object_setIvar(self, ivar, [val copy]);
    }
}

void dl_setIvarForName(id self, const char *name, id val) {
    Ivar ivar = class_getInstanceVariable([self class], name);
    id oldVer = object_getIvar(self, ivar);
    if (oldVer != val) {
        object_setIvar(self, ivar, [val copy]);
    }
}

id dl_getIvar(id self, SEL _cmd) {
    const char *getterVar = [[DLUtils toAccessorVarFromGetter:(NSStringFromSelector(_cmd))] UTF8String];
    Ivar ivar = class_getInstanceVariable([self class], getterVar);
    return object_getIvar(self, ivar);
}

#pragma mark IMP

id dl_initWithPropImp(id self, SEL _cmd, id arg, DLClass *cls) {
    NSString *selStr = NSStringFromSelector(_cmd);
    DLSlot *slot = [cls slotWithInitArg:[[DLKeyword alloc] initWithString:selStr]];
    NSString *propName = [[NSString alloc] initWithFormat:@"_%@", slot.value.value];
    dl_setIvarForName(self, [propName UTF8String], arg);
    return self;
}

id dl_initImp(id self, SEL _cmd) {
    // TODO initialise the object
    return self;
}

id dl_methodImp(id self, SEL _cmd, DreamLisp *dl, DLMethod *method, NSMutableArray *args) {
    DLEnv *env = [[DLEnv alloc] initWithEnv:method.env binds:[method binds] exprs:args];
    id<DLDataProtocol> elem = [dl eval:method.ast withEnv:env];
    return elem;
}

@implementation DLObjcRT {
    Protocol *_dlDataProtocol;
    Protocol *_dlProxyProtocol;
}

- (void)dealloc {
    [DLLog debug:@"DLObjcRT dealloc"];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        [self bootstrap];
    }
    return self;
}

- (void)bootstrap {
    _setterPttn = [NSRegularExpression regularExpressionWithPattern:@"(set)(.*)(:)" options:0 error:nil];
    _dlDataProtocol = [self protocolFromString:@"DLDataProtocol"];
    _dlProxyProtocol = [self protocolFromString:@"DLProxyProtocol"];
}

- (Class)allocateClass:(NSString *)name superclass:(Class _Nullable)superclass {
    return objc_allocateClassPair(superclass, [name UTF8String], 0);
}

/**
 Add an instance variable to a class. This can be called only after allocating a class and before registering. Instance variables cannot be added after a
 class gets registered. A class should not be a metaclass.
 */
- (BOOL)addIVar:(Class)cls name:(NSString *)name type:(NSString *)type {
    size_t size = sizeof(cls);
    return class_addIvar(cls, [name UTF8String], size, log2(size), [type UTF8String]);
}

- (BOOL)addProperty:(Class)cls name:(NSString *)name attr:(DLObjcPropertyAttr *)prop {
    unsigned int count = 0;
    objc_property_attribute_t attrs[10];
    objc_property_attribute_t type = { "T", prop.type };
    attrs[count++] = type;
    objc_property_attribute_t ownership = { "C", "" }; // C = copy
    attrs[count++] = ownership;
    objc_property_attribute_t backingivar  = { "V", prop.backingIvar };
    attrs[count++] = backingivar;
    // TODO: add other attributes from the prop.
    class_addProperty(cls, prop.name, attrs, count);
    return true;
//    unsigned int count = 0;
//    objc_property_attribute_t *attrs = (objc_property_attribute_t *)malloc(128 * sizeof(objc_property_attribute_t));
//    /* Add property type */
//    attrs[count].name = DLObjcPropertyAttrType.shared.attrType;
//    attrs[count].value = [[NSString stringWithFormat:@"T%s", prop.type ? prop.type : @encode(id)] UTF8String];
//    ++count;
//    /* Add property name */
//    attrs[count].name = DLObjcPropertyAttrType.shared.attrValue;
//    attrs[count].value = [prop.value UTF8String];
//    ++count;
//    /* Add read-only  */
//    if (prop.isReadOnly) {
//        attrs[count].name = DLObjcPropertyAttrType.shared.attrReadOnly;
//        ++count;
//    }
//    /* Add retain */
//    if (prop.isRetain) {
//        attrs[count].name = DLObjcPropertyAttrType.shared.attrRetain;
//        ++count;
//    }
//    /* Add non-atomic */
//    if (prop.isNonAtomic) {
//        attrs[count].name = DLObjcPropertyAttrType.shared.attrNonAtomic;
//        ++count;
//    }
//    /* Add getter */
//    attrs[count].name = [[NSString stringWithFormat:@"%s%@", DLObjcPropertyAttrType.shared.attrCustomGetter, prop.customGetter ?
//                          prop.customGetter.string : prop.value] UTF8String];
//    ++count;
//    /* Add setter */
//    if (prop.customSetter) {
//        attrs[count].name = [[NSString stringWithFormat:@"%s%@", DLObjcPropertyAttrType.shared.attrCustomSetter, prop.customSetter.string] UTF8String];
//    } else {
//        attrs[count].name = [[NSString stringWithFormat:@"%s%@", DLObjcPropertyAttrType.shared.attrCustomSetter, [DLUtils toSetterName:(prop.value)]] UTF8String];
//    }
//    ++count;
//    /* Add dynamic */
//    if (prop.isDynamic) {
//        attrs[count].name = DLObjcPropertyAttrType.shared.attrDynamic;
//        ++count;
//    }
//    /* Add weak reference */
//    if (prop.isWeakReference) {
//        attrs[count].name = DLObjcPropertyAttrType.shared.attrWeakReference;
//        ++count;
//    }
//    if (!prop.isRetain && !prop.isDynamic && !prop.isWeakReference) {
//        /* Add copy (default) */
//        //if (prop.isCopy) {
//            attrs[count].name = DLObjcPropertyAttrType.shared.attrCopy;
//            ++count;
//        //}
//    }
//    BOOL ret = class_addProperty(cls, [name UTF8String], attrs, ++count);
//    free(attrs);
//    return ret;
}

/** Returns an array of property attribute type for all properties of the given class. */
- (NSMutableArray *)getPropertiesAttributes:(NSString *)className {
    id cls = objc_getClass([className UTF8String]);
    unsigned int outCount, i;
    objc_property_t *properties = class_copyPropertyList(cls, &outCount);
    NSMutableArray *arr = [NSMutableArray new];
    for (i = 0; i < outCount; i++) {
        objc_property_t property = properties[i];
        [arr addObject:[[NSString alloc] initWithUTF8String:property_getAttributes(property)]];
    }
    return arr;
}


- (BOOL)addMethod:(Class)cls name:(SEL)name imp:(IMP)imp type:(const char *)types {
    return class_addMethod(cls, name, imp, types);
}

/** Registers the given class with Objective-C runtime. Call this method once the class is done building, like after adding properties, methods. */
- (void)registerClass:(Class)class {
    objc_registerClassPair(class);
}

/*!
 Returns the class object if the class is registered with the runtime. If the class is not registered, it calls the class handler callback and check for a
 second time to see if the class is registered. */
- (id _Nullable)getClass:(NSString *)className {
    return objc_getClass([className UTF8String]);
}

/*! Returns a the class if it's registered with the Objective-C runtime, else nil. */
- (Class _Nullable)lookUpClass:(NSString *)className {
    return objc_lookUpClass([className UTF8String]);
}

/** Add protocol conformance to the given class. */
- (BOOL)addDLDataProtocol:(Class)cls proto:(NSString *)proto {
    Protocol *aProto = objc_getProtocol([proto UTF8String]);
    return class_addProtocol(cls, aProto);
}

- (Protocol * _Nullable)protocolFromString:(NSString *)string {
    return objc_getProtocol([string UTF8String]);
}

- (BOOL)conformsToProtocol:(id)object protocolName:(NSString *)name {
    Protocol *proto = [self protocolFromString:name];
    return proto && [object conformsToProtocol:proto];
}

- (IMP _Nullable)implementationFor:(Class)cls selector:(SEL)name {
    return class_getMethodImplementation(cls, name);
}

/*! Allocates an object of the proxy class returning an invocation object which can be used to instantiate the object later. */
- (DLInvocation *)invocationFromClass:(DLClass *)cls forSelector:(SEL)sel args:(NSMutableArray *)args {
    id obj = [cls.proxy alloc];
    NSMethodSignature *sig = [obj methodSignatureForSelector:sel];
    if (!sig) @throw [DLError exceptionWithFormat:DLMethodSignatureNotFoundError, NSStringFromSelector(sel)];
    NSInvocation *invo = [NSInvocation invocationWithMethodSignature:sig];
    invo.target = obj;
    invo.selector = sel;
    NSUInteger len = args.count;
    if (len > 0) {  /* If there are arguments, add it to the invocation object starting from index 2. First two are implicit id, SEL. */
        NSUInteger i = 0;
        while (i < len) {
            id arg = [args objectAtIndex:i];
//            if ([obj conformsToProtocol:_dlDataProtocol]) {
//                arg = [DLUtils convertFromDLTypeToFoundationType:arg];
//            }
            [invo setArgument:&arg atIndex:i + 2];
            i++;
        }
    }
    [invo retainArguments];
    DLInvocation *dlInvo = [DLInvocation new];
    dlInvo.invocation = invo;
    dlInvo.cls = cls;
    dlInvo.args = args;
    return dlInvo;
}

/*! Creates a invocation for the selector with the given proxy, which can be an object or a class with the specified arguments. */
- (DLInvocation *)invocationForMethod:(SEL)selector withProxy:(id<DLProxyProtocol>)proxy args:(NSMutableArray *)args {
    NSMethodSignature *sig = [proxy methodSignatureForSelector:selector];
    DLInvocation *dlInvo = [DLInvocation new];
    if (!sig) {
        [[[DLError alloc] initWithFormat:DLMethodSignatureNotFoundError, NSStringFromSelector(selector)] throw];
        return dlInvo;
    }
    NSInvocation *invo = [NSInvocation invocationWithMethodSignature:sig];
    invo.target = proxy;
    invo.selector = selector;
    NSUInteger len = args.count;
    if (len > 0) {
        NSUInteger i = 0;
        while (i < len) {
            id arg = [args objectAtIndex:i];
            [invo setArgument:&arg atIndex:i + 2];
            i++;
        }
    }
    [invo retainArguments];
    dlInvo.invocation = invo;
    dlInvo.cls = [DLClass isClass:proxy] ? proxy : [(DLObject *)proxy cls];
    dlInvo.args = args;
    return dlInvo;
}

/*! Instantiate the given class using the selector with the give argument. This is to be used only after the selector (method) is defined for the class. */
- (id)instantiate:(DLClass *)cls selector:(SEL)sel args:(NSMutableArray *)args {
    DLInvocation *dlInvo = [self invocationFromClass:cls forSelector:sel args:args];
    NSInvocation *invo = dlInvo.invocation;
    [invo invoke];
    [invo retainArguments];
    id ret = nil;
    [invo getReturnValue:&ret];
    return ret;
}

/*! Instantiate an object from the given invocation object. */
- (id)invoke:(NSInvocation *)invocation {
    [invocation invoke];
    id ret = nil;
    [invocation getReturnValue:&ret];
    return ret;
}

/*!
 Sets an associated object to the given object with the key. This increase the retain count of the associated object. We can use this to also remove the
 association by passing in a null for the associated object, for the same key.
 */
- (void)setAssociatedObject:(id _Nullable)assocObject toObject:(id)object withKey:(const void *)key {
    objc_setAssociatedObject(object, key, assocObject, OBJC_ASSOCIATION_RETAIN);
}

/*! Retrieves an associated object using the given key for object. */
- (id)getAssociatedObject:(id)object forKey:(const void *)key {
    return objc_getAssociatedObject(object, key);
}

/*!
 Returns the encoded type string for the given DL keyword string type
 @link //apple_ref/doc/uid/TP40008048-CH100-SW1 Type Encodings /@link
 */
- (const char *)typeFromKeywordString:(NSString *)string {
    if ([string isEqual:@"char"]) return @encode(char);
    if ([string isEqual:@"int"]) return @encode(int);
    if ([string isEqual:@"short"]) return @encode(short);
    if ([string isEqual:@"long"]) return @encode(long);  /* l is treated as a 32-bit quantity on a 64 bit platform. */
    if ([string isEqual:@"long-long"]) return @encode(long long);
    if ([string isEqual:@"unsigned-char"]) return @encode(unsigned char);
    if ([string isEqual:@"unsigned-int"]) return @encode(unsigned int);
    if ([string isEqual:@"unsigned-short"]) return @encode(unsigned short);
    if ([string isEqual:@"unsigned-long"]) return @encode(unsigned long);
    if ([string isEqual:@"unsigned-long-long"]) return @encode(unsigned long long);
    if ([string isEqual:@"float"]) return @encode(float);
    if ([string isEqual:@"double"]) return @encode(double);
    if ([string isEqual:@"bool"]) return @encode(_Bool);
    if ([string isEqual:@"void"]) return @encode(void);
    if ([string isEqual:@"char*"]) return @encode(char *);
    if ([string isEqual:@"class"]) return @encode(Class);
    if ([string isEqual:@"sel"]) return @encode(SEL);
    return @encode(id);  /* Defaults to an object */
}

@end
