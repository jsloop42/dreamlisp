//
//  DLObjcMethodAttr.h
//  DreamLisp
//
//  Created by jsloop on 13/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLKeyword.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLObjcMethodAttrKey: NSObject
@property (nonatomic, readonly) DLKeyword *kNullable;
@property (nonatomic, readonly) DLKeyword *kWeak;
+ (instancetype)shared;
@end

@interface DLObjcMethodAttr : NSObject
@property (nonatomic, readwrite, assign) BOOL isNullable;
@property (nonatomic, readwrite, retain) DLKeyword *type;
@end

NS_ASSUME_NONNULL_END
