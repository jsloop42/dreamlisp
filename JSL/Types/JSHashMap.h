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
#import "Const.h"
#import "TypeUtils.h"
#import "NSMapTable+JSHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSHashMap : NSObject <JSDataProtocol>
+ (BOOL)isHashMap:(id)object;
+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data fnName:(NSString *)fnName;
+ (JSHashMap *)dataToHashMap:(id<JSDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithMapTable:(NSMapTable *)table;
- (instancetype)initWithArray:(NSArray *)array;
- (instancetype)initWithMeta:(id<JSDataProtocol>)meta hashmap:(JSHashMap *)hashmap;
- (id<JSDataProtocol>)objectForKey:(id)key;
- (void)setObject:(id<JSDataProtocol>)object forKey:(id)key;
- (NSUInteger)count;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (BOOL)containsKey:(id)key;
- (BOOL)isEqual:(id<JSDataProtocol>)hashmap;
- (NSUInteger)hash;
- (NSArray *)sortKeys:(NSInteger (*)(id, id, void *))sorter;
- (NSArray *)sortObjects:(NSInteger (*)(id, id, void *))sorter;
- (NSArray *)sortedKeysUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
- (NSArray *)sortedObjectsUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
@end

NS_ASSUME_NONNULL_END
