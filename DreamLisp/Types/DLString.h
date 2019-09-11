//
//  DLString.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Const.h"
#import "DLDataProtocol.h"
#import "NSString+DLDataProtocol.h"
#import "DLError.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLString : NSObject <DLDataProtocol, NSSecureCoding>
@property (nonatomic, readwrite, retain) NSMutableString *mutableValue;
+ (BOOL)isString:(id)object;
+ (BOOL)isString:(id)object withValue:(NSString *)name;
+ (DLString *)dataToString:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLString *)dataToString:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
+ (DLString *)mutable;
+ (DLString *)stringWithString:(NSString *)string;
- (instancetype)init;
- (instancetype)initWithString:(NSString *)str;
- (instancetype)initWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (instancetype)initWithMutableString;
- (instancetype)initWithMutableString:(NSMutableString *)string;
- (instancetype)initWithArray:(NSMutableArray<DLString *> *)array;
- (instancetype)initWithContentsOfFile:(NSString *)filePath;
- (instancetype)initWithCString:(const char *)string;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta string:(DLString *)string;
- (BOOL)isEmpty;
- (NSUInteger)count;
- (NSString *)substringFrom:(NSInteger)start;
- (NSString *)substringFrom:(NSInteger)start to:(NSInteger)end;
- (NSString *)substringFrom:(NSInteger)start count:(NSInteger)count;
- (DLString *)sort:(NSInteger (*)(id, id, void *))sorter;
- (DLString *)sortedUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
- (DLString *)joined:(NSArray *)arr with:(NSString *)separator;
- (NSString * _Nullable)reverse;
#pragma mark - MutableString methods
- (void)append:(DLString *)string;
- (void)appendString:(NSString *)string;
- (void)append:(id)object atIndex:(NSInteger)index;
@end

NS_ASSUME_NONNULL_END
