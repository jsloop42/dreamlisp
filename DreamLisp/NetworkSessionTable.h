//
//  NetworkSessionTable.h
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTable.h"
#import "NotificationData.h"

NS_ASSUME_NONNULL_BEGIN

@class NotificationData;

@interface NetworkSessionTable: DLTable
+ (NetworkSessionTable *)shared;
- (void)setNotification:(NotificationData *)data;
- (NotificationData * _Nullable)notification:(NSNumber *)notifID;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)new NS_UNAVAILABLE;
@end

NS_ASSUME_NONNULL_END
