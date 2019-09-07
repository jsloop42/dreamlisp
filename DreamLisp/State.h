//
//  State.h
//  DreamLisp
//
//  Created by jsloop on 05/05/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface State : NSObject
+ (NSUInteger)counter;
+ (void)setIsVerbose:(BOOL)flag;
+ (BOOL)isVerbose;
+ (NSString *)currentModuleName;
+ (void)setCurrentModuleName:(NSString *)name;
- (NSUInteger)counter;
- (NSInteger)currentCounter;
@end

NS_ASSUME_NONNULL_END