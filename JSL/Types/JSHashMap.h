//
//  JSHashMap.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "JSError.h"
#import "Logger.h"
#import "Constants.h"
#import "NSMapTable+JSHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSHashMap : NSObject <JSDataProtocol>
+ (BOOL)isHashMap:(id)object;
+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data;
+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data position:(NSInteger)position;
- (instancetype)init;
- (instancetype)initWithMapTable:(NSMapTable *)table;
- (instancetype)initWithArray:(NSArray *)array;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta hashmap:(JSHashMap *)hashmap;
- (id<JSDataProtocol>)objectForKey:(id)key;
- (void)setObject:(id<JSDataProtocol>)object forKey:(id)key;
- (NSUInteger)count;
- (NSMapTable *)value;
- (void)setValue:(NSMapTable *)table;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (BOOL)containsKey:(id)key;
- (BOOL)isEqual:(id<JSDataProtocol>)hashmap;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
