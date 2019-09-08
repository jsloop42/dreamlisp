//
//  NotificationTable.h
//  DreamLisp
//
//  Created by jsloop on 07/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSMapTable+DLHashMap.h"
#import "DLDataProtocol.h"
#import "DLKeyword.h"
#import "DLFunction.h"
#import "DLTable.h"
#import "NotificationData.h"

NS_ASSUME_NONNULL_BEGIN

@class NotificationData;

@interface NotificationTable : DLTable
@property (nonatomic, readwrite) NSMapTable *table;
+ (NotificationTable *)shared;
- (void)setNotification:(NotificationData *)data;
- (NotificationData * _Nullable)notification:(DLKeyword *)notificationKey;
@end

NS_ASSUME_NONNULL_END
