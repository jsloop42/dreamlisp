//
//  DLRegex.h
//  DreamLisp
//
//  Created by Jaseem V V on 07/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLError.h"
#import "DLState.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLRegex : NSObject<DLDataProtocol>
+ (BOOL)isRegex:(id)object;
+ (DLRegex *)dataToRegex:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLRegex *)dataToRegex:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithRegularExpression:(NSRegularExpression *)regex;
@end

NS_ASSUME_NONNULL_END
