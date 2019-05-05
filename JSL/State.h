//
//  State.h
//  JSL
//
//  Created by jsloop on 05/05/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface State : NSObject
+ (NSUInteger)counter;
- (NSUInteger)counter;
- (NSInteger)currentCounter;
@end

NS_ASSUME_NONNULL_END
