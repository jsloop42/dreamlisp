//
//  JSString.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Constants.h"
#import "JSDataProtocol.h"
#import "NSString+JSDataProtocol.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSString : NSObject <JSDataProtocol>
+ (BOOL)isString:(id)object;
+ (JSString *)dataToString:(id<JSDataProtocol>)data;
+ (JSString *)dataToString:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithFormat:(NSString *)format, ...;
- (instancetype)initWithContentsOfFile:(NSString *)filePath;
- (instancetype)initWithCString:(const char *)string;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta string:(JSString *)string;
- (BOOL)isEmpty;
- (NSUInteger)count;
- (BOOL)isEqual:(JSString *)string;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
