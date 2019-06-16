//
//  CacheTable.h
//  JSL
//
//  Created by jsloop on 16/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "NSMutableArray+JSList.h"
#import "NSMapTable+JSHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@interface CacheTable : NSObject
@property (nonatomic, readwrite) NSMapTable *table;
@property (nonatomic, readwrite) NSMutableArray *mru;
@property (nonatomic, readwrite) NSUInteger mruSize;
- (instancetype)init;
- (instancetype)initWithSize:(NSUInteger)size;
- (id _Nullable)objectForKey:(id<JSDataProtocol>)key;
- (void)setObject:(id)object forKey:(id<JSDataProtocol>)key;
- (void)clear;
- (NSUInteger)count;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
@end

NS_ASSUME_NONNULL_END
