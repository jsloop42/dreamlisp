//
//  DLNotificationTable.m
//  DreamLisp
//
//  Created by Jaseem V V on 07/09/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import "DLNotificationTable.h"

static DLNotificationTable *_notifTable;

/** NotitificationTable is used to store notification handler details associated with a notification string. */
@implementation DLNotificationTable

@dynamic table;  // NSMapTable<DLKeyword *, NotificationData *> from the super class.

+ (DLNotificationTable *)shared {
    @synchronized (self) {
        if (!_notifTable) {
            _notifTable = [DLNotificationTable new];
        }
        return _notifTable;
    }
}

- (instancetype)init {
    return [super init];
}

- (void)setNotification:(DLNotificationData *)data {
    [self.table setObject:data forKey:data.notificationKey];
}

- (DLNotificationData * _Nullable)notification:(DLKeyword *)notificationKey {
    return [self.table objectForKey:notificationKey];
}

@end
