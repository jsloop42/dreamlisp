//
//  DLTableProtocol.h
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSMapTable+DLHashMap.h"

NS_ASSUME_NONNULL_BEGIN

@protocol DLTableProtocol <NSObject>
@property (nonatomic, readwrite, retain) NSMapTable *table;
- (NSArray *)allKeys;
- (NSArray *)allObjects;
- (NSUInteger)count;
@end

NS_ASSUME_NONNULL_END
