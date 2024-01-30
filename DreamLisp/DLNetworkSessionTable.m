//
//  DLNetworkSessionTable.m
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "DLNetworkSessionTable.h"

static DLNetworkSessionTable *_networkSessionTable;

/** Network session table is used to store the notification associated with each @c NSURLSession request. */
@implementation DLNetworkSessionTable

@dynamic table;  // NSMapTable<NSNumber *, NotificationData *>;

+ (DLNetworkSessionTable *)shared {
    @synchronized (self) {
        if (!_networkSessionTable) {
            _networkSessionTable = [DLNetworkSessionTable new];
        }
        return _networkSessionTable;
    }
}

- (instancetype)init {
    return [super init];
}

- (void)setNotification:(DLNotificationData *)data {
    [self.table setObject:data forKey:data.identifier];
}

- (DLNotificationData * _Nullable)notification:(NSNumber *)notifID {
    return [self.table objectForKey:notifID];
}

@end
