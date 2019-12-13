//
//  DLCacheTable.h
//  DreamLisp
//
//  Created by jsloop on 16/06/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "NSMutableArray+DLCat.h"
#import "NSMapTable+DLCat.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLCacheTable : NSObject
@property (nonatomic, readwrite, retain) NSMapTable *table;
@property (nonatomic, readwrite, retain) NSMutableArray *mru;
@property (nonatomic, readwrite) NSUInteger mruSize;
- (instancetype)init;
- (instancetype)initWithSize:(NSUInteger)size;
- (id _Nullable)objectForKey:(id)key;
- (void)setObject:(id)object forKey:(id)key;
- (void)clear;
- (NSUInteger)count;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
@end

NS_ASSUME_NONNULL_END