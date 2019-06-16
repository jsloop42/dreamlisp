//
//  Utils.h
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "CacheTable.h"

NS_ASSUME_NONNULL_BEGIN

@interface Utils : NSObject
+ (CacheTable *)cache;
+ (BOOL)matchString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern;
+ (NSArray *)matchesInString:(NSString *)string withExpression:(NSRegularExpression *)pattern;
+ (double)timestamp;
+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object;
+ (NSMutableArray *)toArray:(id<JSDataProtocol>)object isNative:(BOOL)isNative;
- (instancetype)init NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
