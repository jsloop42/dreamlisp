//
//  DLNetwork.h
//  DreamLisp
//
//  Created by jsloop on 08/09/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "NSMapTable+DLCat.h"
#import "DLNotificationTable.h"
#import "DLNotificationData.h"
#import "DLNetworkSessionTable.h"
#import "DLEnv.h"
#import "DLUtils.h"

NS_ASSUME_NONNULL_BEGIN

@class DLNetworkSessionTable;

@interface DLNetwork : NSObject<NSURLSessionDelegate, NSURLSessionTaskDelegate, NSURLSessionDataDelegate>
@property (nonatomic, readwrite, retain) DLEnv *env;
@property (nonatomic, readwrite, retain) DLNetworkSessionTable *networkSessionTable;
@property (nonatomic, readwrite, retain) NSURLSession *urlSession;
@end

NS_ASSUME_NONNULL_END
