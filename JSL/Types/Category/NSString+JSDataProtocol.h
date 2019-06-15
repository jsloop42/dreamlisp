//
//  NSString+JSString.h
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSString (JSDataProtocol)
+ (BOOL)isString:(id)object;
- (BOOL)isEmpty;
- (BOOL)isNotEmpty;
- (NSUInteger)count;
- (NSString *)trim;
- (NSInteger)sortValue;
@end

NS_ASSUME_NONNULL_END
