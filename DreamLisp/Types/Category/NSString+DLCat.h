//
//  NSString+DLString.h
//  DreamLisp
//
//  Created by Jaseem V V on 13/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface NSString (DLCat)
+ (BOOL)isString:(id)object;
- (BOOL)isEmpty;
- (BOOL)isNotEmpty;
- (NSUInteger)count;
- (NSString *)trim;
- (NSInteger)sortValue;
@end

NS_ASSUME_NONNULL_END
