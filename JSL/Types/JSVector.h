//
//  JSVector.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSList.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSVector: JSList <JSDataProtocol>
+ (BOOL)isVector:(id)object;
+ (JSList *)dataToList:(id<JSDataProtocol>)data;
+ (JSList *)dataToList:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta vector:(JSVector *)vector;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (void)enumerateConcurrent:(void (^)(id _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop))block;
- (JSList *)list;
- (BOOL)isEqual:(JSVector *)vector;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
