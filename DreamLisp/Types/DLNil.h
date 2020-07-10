//
//  DLNil.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLNil : NSObject <DLDataProtocol>
+ (BOOL)isNil:(id)object;
- (instancetype)init;
@end

NS_ASSUME_NONNULL_END
