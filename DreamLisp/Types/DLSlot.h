//
//  DLSlot.h
//  DreamLisp
//
//  Created by Jaseem V V on 01/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLSymbol.h"
#import "DLKeyword.h"
#import "DLObjcPropertyAttr.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLSymbol;
@class DLKeyword;

@interface DLSlot: NSObject<DLDataProtocol>
@property (nonatomic, readwrite, retain) DLSymbol *value;  /* slot name*/
@property (nonatomic, readwrite, nullable, retain) id<DLDataProtocol> defaultValue;  /* An optional default value of the property */
/**
 The :initarg which takes a keyword representing the init method that takes the property as it value (like :with-name). If specified, a method will then be
 generated.
 */
@property (nonatomic, readwrite, retain) DLKeyword *initializationArg;
@property (nonatomic, readwrite, retain) DLObjcPropertyAttr *attribute;
// https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtPropertyIntrospection.html#//apple_ref/doc/uid/TP40008048-CH101-SW6
@property (nonatomic, readwrite, assign) const char *propertyType;  /* Objective-C Property Type */
@property (nonatomic, readwrite, assign) const char *methodType;  /* auto synthesize method signature type */
+ (BOOL)isSlot:(id)any;
- (SEL)selectorForInitArg;
@end

NS_ASSUME_NONNULL_END
