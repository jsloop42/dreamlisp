//
//  JSVector.h
//  JSL
//
//  Created by jsloop on 28/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSData.h"
#import "JSList.h"

NS_ASSUME_NONNULL_BEGIN

@interface JSVector: JSList
+ (BOOL)isVector:(id)object;
+ (JSList *)dataToList:(JSData *)data;
+ (JSList *)dataToList:(JSData *)data position:(NSInteger)position;
- (instancetype)initWithArray:(NSArray *)list;
- (instancetype)initWithMeta:(JSData *)meta vector:(JSVector *)vector;
- (JSList *)list;
- (BOOL)isEqual:(JSVector *)vector;
- (NSUInteger)hash;
@end

NS_ASSUME_NONNULL_END
