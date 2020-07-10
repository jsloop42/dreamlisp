//
//  DLBool.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLBool : NSObject <DLDataProtocol>
+ (BOOL)isBool:(id)object;
- (instancetype)initWithBool:(BOOL)flag;
- (instancetype)initWithDLBool:(DLBool *)object;
- (BOOL)value;
@end

NS_ASSUME_NONNULL_END
