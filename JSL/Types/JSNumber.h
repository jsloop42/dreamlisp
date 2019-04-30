//
//  JSNumber.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "Utils.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSNumber : NSObject <JSDataProtocol>
+ (BOOL)isNumber:(id)object;
+ (JSNumber *)dataToNumber:(id<JSDataProtocol>)data;
+ (JSNumber *)dataToNumber:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)initWithDouble:(double)num;
- (instancetype)initWithInt:(int)number;
- (instancetype)initWithInteger:(NSUInteger)number;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithNumber:(NSDecimalNumber *)number;
- (instancetype)initWithDoubleNumber:(NSDecimalNumber *)number;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta number:(JSNumber *)number;
- (double)doubleValue;
- (int)intValue;
- (NSUInteger)integerValue;
- (BOOL)isDouble;
- (NSDecimalNumber *)value;
- (NSString *)string;
- (BOOL)isEqual:(JSNumber *)num;
- (NSUInteger)hash;
- (NSString *)dataTypeName;
@end

NS_ASSUME_NONNULL_END
