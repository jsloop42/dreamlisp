//
//  Network.h
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "NSMapTable+DLHashMap.h"
#import "NotificationTable.h"
#import "NotificationData.h"
#import "NetworkSessionTable.h"
#import "Env.h"
#import "Utils.h"

NS_ASSUME_NONNULL_BEGIN

@class NetworkSessionTable;

@interface Network : NSObject<NSURLSessionDelegate, NSURLSessionTaskDelegate, NSURLSessionDataDelegate>
@property (nonatomic, readwrite, retain) Env *env;
@property (nonatomic, readwrite, retain) NetworkSessionTable *networkSessionTable;
@property (nonatomic, readwrite, retain) NSURLSession *urlSession;
@end

NS_ASSUME_NONNULL_END
