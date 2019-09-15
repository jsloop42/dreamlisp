//
//  DLObjcMethodAttr.m
//  DreamLisp
//
//  Created by jsloop on 13/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLObjcMethodAttr.h"

static DLObjcMethodAttrKey *_objcMethodAttrKey;

@implementation DLObjcMethodAttrKey

+ (instancetype)shared {
    @synchronized (self) {
        if (!_objcMethodAttrKey) {
            _objcMethodAttrKey = [DLObjcMethodAttrKey new];
        }
        return _objcMethodAttrKey;
    }
}

- (void)dealloc {
    [DLLog debug:@"DLObjcMethodAttr dealloc"];
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _kNullable = [DLKeyword keywordWithString:@"nullable"];
        _kWeak = [DLKeyword keywordWithString:@"weak"];
    }
    return self;
}

@end

@implementation DLObjcMethodAttr

@end
