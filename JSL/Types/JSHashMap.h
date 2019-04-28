//
//  JSHashMap.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"
#import "JSError.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSHashMap : JSData
@property (nonatomic, readwrite, copy) JSData *meta;
- (instancetype)init;
- (instancetype)initWithMapTable:(NSMapTable *)table;
- (instancetype)initWithArray:(NSArray *)array;
- (instancetype)initWithMeta:(JSData *)meta hashmap:(JSHashMap *)hashmap;
- (JSData *)objectForKey:(id)key;
- (void)setObject:(JSData *)object forKey:(id)key;
- (NSUInteger)count;
- (NSMapTable *)value;
- (void)setValue:(NSMapTable *)table;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (BOOL)containsKey:(id)key;
- (BOOL)isEqual:(JSData *)hashmap;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
