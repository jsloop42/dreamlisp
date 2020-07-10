//
//  DLCore.h
//  DreamLisp
//
//  Created by Jaseem V V on 05/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTypes.h"
#import "DLDelegate.h"
#import "DLReader.h"
#import "DLPrinter.h"
#import "DLLogger.h"
#import "DLModuleTable.h"
#import "DLEnv.h"
#import <objc/message.h>
#import "DLAlgorithm.h"
#import "DLFileOps.h"
#import "DLNotificationTable.h"

NS_ASSUME_NONNULL_BEGIN

@interface DLCore : NSObject
@property (nonatomic, weak) id<DLDelegate> delegate;
- (DLEnv *)env;
@end

NS_ASSUME_NONNULL_END
