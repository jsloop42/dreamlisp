//
//  DLInvocationArgument.h
//  DreamLisp
//
//  Created by jsloop on 03/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLInvocationArgument : NSObject
@property (nonatomic, readwrite, retain) NSMutableArray<id> *args;
@property (nonatomic, readwrite, assign) NSString *type;
@end

NS_ASSUME_NONNULL_END
