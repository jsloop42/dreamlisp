//
//  DLHashMap.h
//  DreamLisp
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "JSError.h"
#import "Logger.h"
#import "Const.h"
#import "TypeUtils.h"
#import "NSMapTable+JSHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLHashMap : NSObject <DLDataProtocol>
+ (BOOL)isHashMap:(id)object;
+ (DLHashMap *)dataToHashMap:(id<DLDataProtocol>)data fnName:(NSString *)fnName;
+ (DLHashMap *)dataToHashMap:(id<DLDataProtocol>)data position:(NSInteger)position fnName:(NSString *)fnName;
- (instancetype)init;
- (instancetype)initWithMapTable:(NSMapTable *)table;
- (instancetype)initWithArray:(NSArray *)array;
- (instancetype)initWithMeta:(id<DLDataProtocol>)meta hashmap:(DLHashMap *)hashmap;
- (void)fromArray:(NSArray *)array;
- (id<DLDataProtocol>)objectForKey:(id)key;
- (void)setObject:(id<DLDataProtocol>)object forKey:(id)key;
- (NSUInteger)count;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (BOOL)containsKey:(id)key;
- (NSArray *)sortKeys:(NSInteger (*)(id, id, void *))sorter;
- (NSArray *)sortObjects:(NSInteger (*)(id, id, void *))sorter;
- (NSArray *)sortedKeysUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
- (NSArray *)sortedObjectsUsingComparator:(NSComparisonResult (^)(id obj1, id obj2))comparator;
@end

NS_ASSUME_NONNULL_END