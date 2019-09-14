//
//  DLNotificationTable.h
//  DreamLisp
//
//  Created by jsloop on 07/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NSMapTable+DLCat.h"
#import "DLDataProtocol.h"
#import "DLKeyword.h"
#import "DLFunction.h"
#import "DLTable.h"
#import "DLNotificationData.h"

NS_ASSUME_NONNULL_BEGIN

@class DLNotificationData;

@interface DLNotificationTable : DLTable
@property (nonatomic, readwrite, retain) NSMapTable *table;
+ (DLNotificationTable *)shared;
- (void)setNotification:(DLNotificationData *)data;
- (DLNotificationData * _Nullable)notification:(DLKeyword *)notificationKey;
@end

NS_ASSUME_NONNULL_END
