//
//  DLObjcRT.h
//  DreamLisp
//
//  Created by Jaseem V V on 30/08/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <objc/runtime.h>
#import "DLObjcPropertyAttr.h"
#import "DLInvocation.h"
#import "DLUtils.h"
#import "DLLogger.h"
#import "DreamLisp.h"
#import "DLProxyProtocol.h"

//! Project version number for DreamLisp.
FOUNDATION_EXPORT double DreamLispVersionNumber;

//! Project version string for DreamLisp.
FOUNDATION_EXPORT const unsigned char DreamLispVersionString[];

NS_ASSUME_NONNULL_BEGIN

@class DLInvocation;
@class DLClass;
@class DreamLisp;

extern void dl_setIvarForName(id self, const char *name, id val);
extern void dl_setIvar(id self, SEL _cmd, id ver);
extern id dl_getIvar(id self, SEL _cmd);
extern id dl_initWithPropImp(id self, SEL _cmd, DLClass *cls, id arg);
extern id dl_initImp(id self, SEL _cmd);
extern id dl_methodImp(id self, SEL _cmd, DreamLisp *dl, DLMethod *method, NSMutableArray *args);

@interface DLObjcRT : NSObject
- (Class)allocateClass:(NSString *)name superclass:(Class _Nullable)superclass;
- (void)registerClass:(Class)cls;
- (id _Nullable)getClass:(NSString *)className;
- (Class _Nullable)lookUpClass:(NSString *)className;
- (BOOL)addIVar:(Class)cls name:(NSString *)name type:(NSString *)type;
- (BOOL)addProperty:(Class)cls name:(NSString *)name attr:(DLObjcPropertyAttr *)prop;
- (NSMutableArray *)getPropertiesAttributes:(NSString *)className;
- (BOOL)addMethod:(Class)cls name:(SEL)name imp:(IMP)imp type:(const char *)types;
- (BOOL)addDLDataProtocol:(Class)cls proto:(NSString *)proto;
- (Protocol * _Nullable)protocolFromString:(NSString *)string;
- (BOOL)conformsToProtocol:(id)object protocolName:(NSString *)name;
- (IMP _Nullable)implementationFor:(Class)cls selector:(SEL)name;
- (DLInvocation *)invocationFromClass:(DLClass *)cls forSelector:(SEL)sel args:(NSMutableArray *)args;
- (DLInvocation *)invocationForMethod:(SEL)selector withProxy:(id<DLProxyProtocol>)proxy args:(NSMutableArray *)args;
- (id)instantiate:(DLClass *)cls selector:(SEL)sel args:(NSMutableArray *)args;
- (id)invoke:(NSInvocation *)invocation;
- (void)setAssociatedObject:(id _Nullable)assocObject toObject:(id)object withKey:(const void *)key;
- (id)getAssociatedObject:(id)object forKey:(const void *)key;
- (const char *)typeFromKeywordString:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
