//
//  NSMutableSet+DLSet.h
//  DreamLisp
//
//  Created by Jaseem V V on 24.07.2024.
//  Copyright © 2024 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSMutableSet (DLSet)
- (BOOL)isEmpty;
- (nonnull id)copyWithZone:(nullable NSZone *)zone;
- (nonnull id)mutableCopyWithZone:(nullable NSZone *)zone;
@end

NS_ASSUME_NONNULL_END
