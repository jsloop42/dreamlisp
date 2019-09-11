//
//  DLInvocation.h
//  DreamLisp
//
//  Created by jsloop on 04/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLClass.h"
#import "DLObject.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@class DLClass;
@class DLObject;

@interface DLInvocation : NSObject
@property (nonatomic, readwrite, retain) NSInvocation *invocation;
@property (nonatomic, readwrite, retain) NSMutableArray *args;
@property (nonatomic, readwrite, retain) DLClass *cls;
@property (nonatomic, readwrite, retain) DLObject *object;
@end

NS_ASSUME_NONNULL_END
