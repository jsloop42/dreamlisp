//
//  DLInvocationArgument.h
//  DreamLisp
//
//  Created by Jaseem V V on 03/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

// NB: is not being used right now. The initial purpose was to convert a DL type to NS type with the args array[0] holding the value.
// The type string indicates the type of the argument
@interface DLInvocationArgument : NSObject
@property (nonatomic, readwrite, retain) NSMutableArray<id> *args;
@property (nonatomic, readwrite, assign) NSString *type;
@end

NS_ASSUME_NONNULL_END
