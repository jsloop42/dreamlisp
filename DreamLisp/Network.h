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
@property (nonatomic, readwrite) Env *env;
@property (nonatomic, readwrite) NetworkSessionTable *networkSessionTable;
@property (nonatomic, readwrite) NSURLSession *urlSession;
@end

NS_ASSUME_NONNULL_END
