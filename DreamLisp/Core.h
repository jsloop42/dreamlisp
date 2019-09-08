//
//  Core.h
//  DreamLisp
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "DLDelegate.h"
#import "Reader.h"
#import "Printer.h"
#import "Logger.h"
#import "ModuleTable.h"
#import "Env.h"
#import <objc/message.h>
#import "Algorithm.h"
#import "FileOps.h"
#import "NotificationTable.h"

NS_ASSUME_NONNULL_BEGIN

@interface Core : NSObject
@property (nonatomic, weak) id<DLDelegate> delegate;
- (Env *)env;
@end

NS_ASSUME_NONNULL_END
