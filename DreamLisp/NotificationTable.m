//
//  NotificationTable.m
//  DreamLisp
//
//  Created by jsloop on 07/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import "NotificationTable.h"

static NotificationTable *_notifTable;

/** NotitificationTable is used to store notification handler details associated with a notification string. */
@implementation NotificationTable

@dynamic table;  // NSMapTable<DLKeyword *, NotificationData *> from the super class.

+ (NotificationTable *)shared {
    @synchronized (self) {
        if (!_notifTable) {
            _notifTable = [NotificationTable new];
        }
        return _notifTable;
    }
}

- (instancetype)init {
    return [super init];
}

- (void)setNotification:(NotificationData *)data {
    [self.table setObject:data forKey:data.notificationKey];
}

- (NotificationData * _Nullable)notification:(DLKeyword *)notificationKey {
    return [self.table objectForKey:notificationKey];
}

@end
