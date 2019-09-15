//
//  DLNil.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLNil : NSObject <DLDataProtocol>
+ (BOOL)isNil:(id)object;
- (instancetype)init;
@end

NS_ASSUME_NONNULL_END
