//
//  DLMethod.h
//  DreamLisp
//
//  Created by jsloop on 13/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLFunction.h"
#import "DLSymbol.h"
#import "DLKeyword.h"
#import "DLString.h"
#import "DLObjcMethodAttr.h"
#import "DLClass.h"
#import "DLEnv.h"
#import "DLMethod.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLClass;

@interface DLMethodParam : NSObject<DLDataProtocol>
/*! Parameterized argument name for second args onwards. */
@property (nonatomic, readwrite, retain) DLKeyword *selectorName;
/*! Param name. There will be either a selector name or the arg name. */
@property (nonatomic, readwrite, retain) DLSymbol *name;
@property (nonatomic, readwrite, retain) DLObjcMethodAttr *attr;
/*! A method argument value when used in parsing method invocation form. */
@property (nonatomic, readwrite, retain) id<DLDataProtocol> value;
+ (BOOL)isMethodParam:(id)any;
- (NSString *)string;
@end

@interface DLMethod : NSObject<DLDataProtocol>
/*! The first part of the method selector. */
@property (nonatomic, readwrite, retain) DLSymbol *name;
/*!
 The method params which includes the selector, value or argument name in order as it appears in the expression. If used with @c defmethod parsing, we will
 have only the name, and if used with method invocation expression parsing, the params will have argument value instead. */
@property (nonatomic, readwrite, retain) NSMutableArray <DLMethodParam *> *params;
/*!
 The ast associated with the method, if it's a user define one. For methods that represents selectors of external classes, like from Foundation, there won't
 be any ast. */
@property (nonatomic, readwrite, retain) id<DLDataProtocol> ast;

//@property (nonatomic, readwrite, retain) DLFunction *fn;
/*! The env associated with the method. */
@property (nonatomic, readwrite, retain) DLEnv *env;
/*! Indicates whether the method represents a user defined one or an external method selector. */
@property (nonatomic, readwrite, assign) BOOL isNative;
/*! The method return attribute */
@property (nonatomic, readwrite, retain) DLObjcMethodAttr *attr;
@property (nonatomic, readwrite, weak) DLClass *cls;
/*! The method selector, mainly used for printing. */
@property (nonatomic, readwrite, retain) DLString *selectorString;
@property (nonatomic, readwrite, assign, nonnull) SEL selector;
@property (nonatomic, readwrite, retain) NSMethodSignature *signature;
/*! We store the IMP so that we have the reference to the initially defined IMP. */
@property (nonatomic, readwrite, assign, nonnull) IMP imp;
/*! In case of native function, we store the method type as in ObjCType format. */
@property (nonatomic, readwrite, assign) const char *type;
+ (BOOL)isMethod:(id)any;
- (NSMutableArray *)binds;
- (NSMutableArray *)args;
@end

NS_ASSUME_NONNULL_END
