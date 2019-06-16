//
//  JSNil.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSNil : NSObject <JSDataProtocol>
+ (BOOL)isNil:(id)object;
- (instancetype)init;
@end

NS_ASSUME_NONNULL_END
