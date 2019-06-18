//
//  JSString.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Const.h"
#import "JSDataProtocol.h"
#import "NSString+JSDataProtocol.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSString : NSObject <JSDataProtocol>
@property (nonatomic, readwrite) BOOL isMutable;
@property (nonatomic, readwrite) NSMutableString *mutableValue;
+ (BOOL)isString:(id)object;
+ (BOOL)isString:(id)object withValue:(NSString *)name;
+ (JSString *)dataToString:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSString *)dataToString:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
+ (JSString *)mutable;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithFormat:(NSString *)format, ...;
- (instancetype)initWithMutableString;
- (instancetype)initWithMutableString:(NSMutableString *)string;
- (instancetype)initWithContentsOfFile:(NSString *)filePath;
- (instancetype)initWithCString:(const char *)string;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta string:(JSString *)string;
- (BOOL)isEmpty;
- (NSUInteger)count;
- (JSString *)sort:(NSInteger (*)(id, id, void *))sorter;
- (JSString *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
- (JSString *)joined:(NSArray *)arr with:(NSString *)separator;
#pragma mark - MutableString methods
- (void)append:(JSString *)string;
- (void)appendString:(NSString *)string;
@end

NS_ASSUME_NONNULL_END
