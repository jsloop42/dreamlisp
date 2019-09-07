//
//  DLNumber.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "TypeUtils.h"
#import "DLError.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLNumber : NSObject <DLDataProtocol>
+ (instancetype)new NS_UNAVAILABLE;
+ (BOOL)isNumber:(id)object;
+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data;
+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data position:(NSInteger)position;
+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLNumber *)dataToNumber:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithDouble:(double)num;
- (instancetype)initWithInt:(int)number;
- (instancetype)initWithInteger:(NSInteger)number;
- (instancetype)initWithString:(NSString *)string;
- (instancetype)initWithNumber:(NSDecimalNumber *)number;
- (instancetype)initWithDoubleNumber:(NSDecimalNumber *)number;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta number:(DLNumber *)number;
- (double)doubleValue;
- (int)intValue;
- (NSInteger)integerValue;
- (BOOL)isDouble;
- (NSString *)string;
- (NSString *)dataTypeName;
@end

NS_ASSUME_NONNULL_END