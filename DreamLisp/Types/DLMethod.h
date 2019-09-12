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
#import "DLObjcMethodAttr.h"
#import "DLClass.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLClass;

@interface DLMethodParam: NSObject<DLDataProtocol>
/*! Parameterized argument name for second args onwards. */
@property (nonatomic, readwrite, retain) DLKeyword *selectorName;
/*! Param name */
@property (nonatomic, readwrite, retain) DLSymbol *name;
@property (nonatomic, readwrite, retain) DLObjcMethodAttr *attr;
+ (BOOL)isMethodParam:(id)any;
@end

@interface DLMethod : NSObject<DLDataProtocol>
@property (nonatomic, readwrite, retain) DLSymbol *name;
@property (nonatomic, readwrite, retain) NSMutableArray <DLMethodParam *> *params;
@property (nonatomic, readwrite, retain) DLFunction *fn;
/*! The method return attribute  */
@property (nonatomic, readwrite, retain) DLObjcMethodAttr *attr;
@property (nonatomic, readwrite, retain) DLClass *cls;
@end

NS_ASSUME_NONNULL_END
