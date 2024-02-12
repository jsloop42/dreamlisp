//
//  DLLogger.h
//  DreamLisp
//
//  Created by Jaseem V V on 12/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLIOService.h"

NS_ASSUME_NONNULL_BEGIN

#define DLLog DLLogger

@interface DLLogger : NSObject
+ (BOOL)isDebug;
+ (BOOL)isVerbose;
+ (void)setIsDebug:(BOOL)flag;
+ (void)setIsVerbose:(BOOL)flag;
+ (void)setIOService:(DLIOService *)ioService;
+ (void)info:(NSString *)message;
+ (void)infoWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
+ (void)debug:(NSString *)message;
+ (void)debugWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
+ (void)error:(NSString *)message;
+ (void)errorWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
+ (void)verbose:(NSString *)message;
+ (void)verboseWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
@end

NS_ASSUME_NONNULL_END
