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

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLClass;

@interface DLMethodParam : NSObject<DLDataProtocol>
/*! Parameterized argument name for second args onwards. */
@property (nonatomic, readwrite, retain) DLKeyword *selectorName;
/*! Param name */
@property (nonatomic, readwrite, retain) DLSymbol *name;
@property (nonatomic, readwrite, retain) DLObjcMethodAttr *attr;
+ (BOOL)isMethodParam:(id)any;
- (NSString *)string;
@end

@interface DLMethod : NSObject<DLDataProtocol>
@property (nonatomic, readwrite, retain) DLSymbol *name;
@property (nonatomic, readwrite, retain) NSMutableArray <DLMethodParam *> *params;
@property (nonatomic, readwrite, retain) id<DLDataProtocol> ast;
@property (nonatomic, readwrite, retain) DLFunction *fn;
@property (nonatomic, readwrite, retain) DLEnv *env;
/*! The method return attribute */
@property (nonatomic, readwrite, retain) DLObjcMethodAttr *attr;
@property (nonatomic, readwrite, weak) DLClass *cls;
@property (nonatomic, readwrite, retain) DLString *selectorString;
@property (nonatomic, readwrite, assign, nonnull) SEL selector;
@property (nonatomic, readwrite, retain) NSMethodSignature *signature;
@property (nonatomic, readwrite, assign, nonnull) IMP imp;
+ (BOOL)isMethod:(id)any;
- (NSMutableArray *)binds;
@end

NS_ASSUME_NONNULL_END
