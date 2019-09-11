//
//  NotificationData.h
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLFunction.h"
#import "DLKeyword.h"

NS_ASSUME_NONNULL_BEGIN

@interface NotificationData: NSObject
/* Any type of uniquely identifying info to distinguish the caller state. For NSURLSession, the identifier is used to */
@property (nonatomic, readwrite, retain) NSNumber *identifier;
/** The NSNotificationName associated with the message, which is also the table key in the @c NotificationTable. */
@property (nonatomic, readwrite, retain) DLKeyword *notificationKey;
/** The notification handler function. */
@property (nonatomic, readwrite, retain) DLFunction *notificationHandler;
@end

NS_ASSUME_NONNULL_END
