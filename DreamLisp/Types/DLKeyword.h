//
//  DLKeyword.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLKeyword : NSObject <DLDataProtocol>
+ (BOOL)isKeyword:(id)object;
+ (BOOL)isEncodedKeyword:(id)object;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithKeyword:(NSString *)string;
- (instancetype)initWithEncodedKeyword:(NSString *)keyword;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta keyword:(DLKeyword *)keyword;
- (NSString *)string;
- (NSString *)encoded;
- (NSString *)decoded;
@end

NS_ASSUME_NONNULL_END
