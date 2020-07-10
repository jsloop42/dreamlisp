//
//  State.h
//  DreamLisp
//
//  Created by Jaseem V V on 05/05/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface DLState : NSObject
+ (NSUInteger)counter;
+ (void)setIsVerbose:(BOOL)flag;
+ (BOOL)isVerbose;
+ (NSString *)currentModuleName;
+ (void)setCurrentModuleName:(NSString *)name;
- (NSUInteger)counter;
- (NSInteger)currentCounter;
@end

NS_ASSUME_NONNULL_END
