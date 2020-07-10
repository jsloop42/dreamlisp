//
//  DLKeyword.h
//  DreamLisp
//
//  Created by Jaseem V V on 28/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "DLError.h"
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLKeyword : NSObject <DLDataProtocol>
+ (BOOL)isKeyword:(id)object;
+ (BOOL)isEncodedKeyword:(id)object;
+ (DLKeyword *)dataToKeyword:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLKeyword *)dataToKeyword:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
+ (DLKeyword *)keywordWithString:(NSString *)string;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithKeyword:(NSString *)string;
- (instancetype)initWithEncodedKeyword:(NSString *)keyword;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta keyword:(DLKeyword *)keyword;
- (NSString *)string;
- (NSString *)encoded;
- (NSString *)decoded;
- (BOOL)isEqualToString:(id)string;
@end

NS_ASSUME_NONNULL_END
