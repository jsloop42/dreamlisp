//
//  JSKeyword.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSKeyword : NSObject <JSDataProtocol>
+ (BOOL)isKeyword:(id)object;
+ (BOOL)isEncodedKeyword:(id)object;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithKeyword:(NSString *)string;
- (instancetype)initWithEncodedKeyword:(NSString *)keyword;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta keyword:(JSKeyword *)keyword;
- (NSString *)string;
- (NSString *)encoded;
- (NSString *)decoded;
@end

NS_ASSUME_NONNULL_END
