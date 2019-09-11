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

NS_ASSUME_NONNULL_BEGIN

@class DLClass;
@class DLObject;
@class DLInvocation;

@interface DLObjc : NSObject
- (void)defclass:(DLClass *)cls;
- (DLClass *)parseClassForm:(id<DLDataProtocol>)ast;
- (DLObject *)makeInstance:(DLInvocation *)invocation;
- (DLInvocation *)parseMakeInstanceForm:(id<DLDataProtocol>)ast withEnv:(DLEnv *)env;
@end

NS_ASSUME_NONNULL_END
