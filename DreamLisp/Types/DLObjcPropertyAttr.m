//
//  DLObjcPropertyAttr.m
//  DreamLisp
//
//  Created by Jaseem V V on 02/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLObjcPropertyAttr.h"

static DLObjcPropertyAttrType *propType;

@implementation DLObjcPropertyAttrType

- (void)dealloc {
    [DLLog debug:@"DLObjcPropertyAttrType dealloc"];
}

+ (instancetype)shared {
    if (propType) return propType;
    propType = [DLObjcPropertyAttrType new];
    return propType;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _attrType = "T";
        _attrValue = "V";
        _attrCopy = "C";
        _attrRetain = "&";
        _attrNonAtomic = "N";
        _attrCustomGetter = "G";
        _attrCustomSetter = "S";
        _attrDynamic = "D";
        _attrWeakReference = "W";
    }
    return self;
}

@end

@implementation DLObjcPropertyAttr

- (void)dealloc {
    [DLLog debug:@"DLObjcPropertyAttr dealloc"];
}

@end
