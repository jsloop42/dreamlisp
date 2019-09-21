//
//  DLObjc.h
//  DreamLisp
//
//  Created by jsloop on 31/08/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLEnv.h"
#import "DLObjcRT.h"
#import "DLObjcPropertyAttr.h"
#import "DLDataProtocol.h"
#import "DLLogger.h"
#import "DLClass.h"
#import "DLObject.h"
#import "DLSymbol.h"
#import "DLKeyword.h"
#import "DLError.h"
#import "DLInvocation.h"
#import "DLTypeUtils.h"
#import "DLMethod.h"
#import "DLObjcMethodAttr.h"
#import "DLState.h"

NS_ASSUME_NONNULL_BEGIN

@class DLClass;
@class DLObject;
@class DLInvocation;
@class DLMethod;

@interface DLObjc : NSObject
- (BOOL)isClassExists:(NSString *)className;
- (void)defclass:(DLClass *)cls;
- (DLClass *)parseClassForm:(id<DLDataProtocol>)ast;
- (id<DLDataProtocol>)invokeMethod:(SEL)selector withObject:(id<DLProxyProtocol>)object args:(NSMutableArray *)args;
- (void)invokeMethodWithReturn:(SEL)selector withObject:(id<DLProxyProtocol>)object args:(NSMutableArray *)args;
- (void)addMethodToClass:(DLMethod *)method;
- (DLMethod *)parseMethod:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env;
- (DLObject * _Nullable)makeInstance:(DLInvocation *)invocation;
- (DLInvocation * _Nullable)parseMakeInstanceForm:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env;
@end

NS_ASSUME_NONNULL_END
