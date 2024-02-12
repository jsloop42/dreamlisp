//
//  NSDecimalNumber+DLDataProtocol.h
//  DreamLisp
//
//  Created by Jaseem V V on 13/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSDecimalNumber (DLDataProtocol)
+ (NSString *)dataType;
- (NSString *)dataType;
@end

NS_ASSUME_NONNULL_END
