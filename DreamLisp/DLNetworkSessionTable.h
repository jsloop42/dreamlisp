//
//  DLNetworkSessionTable.h
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTable.h"
#import "DLNotificationData.h"

NS_ASSUME_NONNULL_BEGIN

@class DLNotificationData;

@interface DLNetworkSessionTable: DLTable
+ (DLNetworkSessionTable *)shared;
- (void)setNotification:(DLNotificationData *)data;
- (DLNotificationData * _Nullable)notification:(NSNumber *)notifID;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)new NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
