//
//  NetworkSessionTable.m
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import "NetworkSessionTable.h"

static NetworkSessionTable *_networkSessionTable;

/** Network session table is used to store the notification associated with each @c NSURLSession request. */
@implementation NetworkSessionTable

@dynamic table;  // NSMapTable<NSNumber *, NotificationData *>;

+ (NetworkSessionTable *)shared {
    @synchronized (self) {
        if (!_networkSessionTable) {
            _networkSessionTable = [NetworkSessionTable new];
        }
        return _networkSessionTable;
    }
}

- (instancetype)init {
    return [super init];
}

- (void)setNotification:(NotificationData *)data {
    [self.table setObject:data forKey:data.identifier];
}

- (NotificationData * _Nullable)notification:(NSNumber *)notifID {
    return [self.table objectForKey:notifID];
}

@end
