//
//  Utils.h
//  JSL
//
//  Created by jsloop on 06/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

#ifdef DEBUG
#   define debug(...) NSLog(__VA_ARGS__)
#   define info(...) NSLog(__VA_ARGS__)
#else
#   define info(...) NSLog(__VA_ARGS__)
#endif

@interface Utils : NSObject
+ (BOOL)matchString:(NSString *)string withPattern:(NSString *)pattern;
@end

NS_ASSUME_NONNULL_END
