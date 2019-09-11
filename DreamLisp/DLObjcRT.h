//
//  DLObjcRT.h
//  DreamLisp
//
//  Created by jsloop on 30/08/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <objc/objc-runtime.h>
#import "DLObjcPropertyAttr.h"
#import "DLInvocation.h"
#import "DLUtils.h"
#import "DLLogger.h"

//! Project version number for DreamLisp.
FOUNDATION_EXPORT double DreamLispVersionNumber;

//! Project version string for DreamLisp.
FOUNDATION_EXPORT const unsigned char DreamLispVersionString[];

NS_ASSUME_NONNULL_BEGIN

@class DLInvocation;
@class DLClass;

extern void setIvarForName(id self, const char *name, id val);
extern void setIvar(id self, SEL _cmd, id ver);
extern id getIvar(id self, SEL _cmd);
extern id initWithPropImp(id self, SEL _cmd, DLClass *cls, id arg);
extern id initImp(id self, SEL _cmd);

@interface DLObjcRT : NSObject
- (Class)allocateClass:(NSString *)name superclass:(Class _Nullable)superclass;
- (void)registerClass:(Class)cls;
- (BOOL)addIVar:(Class)cls name:(NSString *)name type:(NSString *)type;
- (BOOL)addProperty:(Class)cls name:(NSString *)name attr:(DLObjcPropertyAttr *)prop;
- (NSMutableArray *)getPropertiesAttributes:(NSString *)className;
- (BOOL)addMethod:(Class)cls name:(SEL)name imp:(IMP)imp type:(const char *)types;
- (BOOL)addDLDataProtocol:(Class)cls proto:(NSString *)proto;
- (Protocol * _Nullable)protocolFromString:(NSString *)string;
- (BOOL)conformsToProtocol:(id)object protocolName:(NSString *)name;
- (IMP _Nullable)implementationFor:(Class)cls selector:(SEL)name;
- (DLInvocation *)invocationFromClass:(DLClass *)cls forSelector:(SEL)sel args:(NSMutableArray *)args;
- (id)instantiate:(DLClass *)cls selector:(SEL)sel args:(NSMutableArray *)args;
- (id)instantiateFromInvocation:(NSInvocation *)invocation;
- (const char *)typeFromKeywordString:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
