//
//  NSMapTable+JSHashMap.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSMapTable (JSHashMap)
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (NSMapTable *)assoc:(NSMapTable *)table;
- (NSMapTable *)dissoc:(NSArray *)keys;
- (BOOL)containsKey:(id)key;
- (void)merge:(NSMapTable *)table;
- (void)updateObject:(id)object forKey:(id)key;
@end

NS_ASSUME_NONNULL_END
