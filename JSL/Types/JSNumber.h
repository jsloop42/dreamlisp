//
//  JSNumber.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"
#import "Utils.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSNumber : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)initWithDouble:(double)num;
- (instancetype)initWithInt:(int)number;
- (instancetype)initWithInteger:(NSUInteger)number;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithNumber:(NSDecimalNumber *)number;
- (instancetype)initWithMeta:(JSData *)meta number:(JSNumber *)number;
- (double)doubleValue;
- (int)intValue;
- (NSUInteger)integerValue;
- (BOOL)isDouble;
- (NSDecimalNumber *)value;
- (NSString *)string;
- (BOOL)isEqual:(JSNumber *)num;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
