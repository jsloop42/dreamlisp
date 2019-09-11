//
//  DLObject.h
//  DreamLisp
//
//  Created by jsloop on 01/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "DLClass.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLClass;

@interface DLObject : NSObject <DLDataProtocol>
@property (nonatomic, readwrite, retain) DLClass *cls;
@property (nonatomic, readwrite, retain) id value;  /* The object that the class holds */
+ (BOOL)isObject:(id)any;
- (instancetype)initWithObject:(id)object;
@end

NS_ASSUME_NONNULL_END
