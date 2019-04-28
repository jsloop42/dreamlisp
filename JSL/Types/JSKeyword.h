//
//  JSKeyword.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSKeyword : JSData
@property (nonatomic, readwrite, copy) NSString *value;
+ (BOOL)isKeyword:(id)string;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithKeyword:(NSString *)string;
- (instancetype)initWithEncodedKeyword:(NSString *)keyword;
- (instancetype)initWithMeta:(JSData *)meta keyword:(JSKeyword *)keyword;
- (NSString *)string;
- (NSString *)encoded;
- (NSString *)decoded;
- (BOOL)isEqual:(JSKeyword *)keyword;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
