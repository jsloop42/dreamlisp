//
//  DLObjcPropertyAttr.h
//  DreamLisp
//
//  Created by jsloop on 02/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLKeyword.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLObjcPropertyAttrType: NSObject
@property (nonatomic, readonly) const char *attrType;
@property (nonatomic, readonly) const char *attrValue;
@property (nonatomic, readonly) const char *attrReadOnly;
@property (nonatomic, readonly) const char *attrCopy;
@property (nonatomic, readonly) const char *attrRetain;
@property (nonatomic, readonly) const char *attrNonAtomic;
@property (nonatomic, readonly) const char *attrCustomGetter;
@property (nonatomic, readonly) const char *attrCustomSetter;
@property (nonatomic, readonly) const char *attrDynamic;
@property (nonatomic, readonly) const char *attrWeakReference;
@property (nonatomic, readonly) const char *attrEligibleForGC;
+ (instancetype)shared;
@end

/*!
    Represent the @textblock attributes for a given Objective-C property.
    @link //apple_ref/doc/uid/TP40008048-CH101-SW6 Declared Property Type Encoding @/link
 */
@interface DLObjcPropertyAttr : NSObject
/*! Type of the property. */
@property (nonatomic, readwrite, assign) const char *type;
/*! The property name. */
@property (nonatomic, readwrite, assign) NSString *value;
@property (nonatomic, readwrite, assign) const char *name;
@property (nonatomic, readwrite, assign) const char *backingIvar;
/*! Custom getter name. */
@property (nonatomic, readwrite, nullable, retain) DLKeyword *customGetter;
/*! Getter name SEL string */
@property (nonatomic, readwrite, assign) const char *getterName;
/*! Custom setter name. */
@property (nonatomic, readwrite, nullable, retain) DLKeyword *customSetter;
/*! Setter name SEL string */
@property (nonatomic, readwrite, assign) const char *setterName;
@property (nonatomic, readwrite, assign) BOOL isReadOnly;
@property (nonatomic, readwrite, assign) BOOL isCopy;
@property (nonatomic, readwrite, assign) BOOL isRetain;
@property (nonatomic, readwrite, assign) BOOL isNonAtomic;
@property (nonatomic, readwrite, assign) BOOL hasCustomGetter;
@property (nonatomic, readwrite, assign) BOOL hasCustomSetter;
@property (nonatomic, readwrite, assign) BOOL isDynamic;
@property (nonatomic, readwrite, assign) BOOL isWeakReference;
@property (nonatomic, readwrite, assign) BOOL isEligibleForGC;
@property (nonatomic, readwrite, assign) NSString *oldStyleTypeEncoding;
@end

NS_ASSUME_NONNULL_END
