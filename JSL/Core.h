//
//  Core.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "JSLDelegate.h"
#import "Reader.h"
#import "Printer.h"
#import "Logger.h"
#import "ModuleTable.h"
#import "Env.h"
#import <objc/message.h>
#import "Algorithm.h"

NS_ASSUME_NONNULL_BEGIN

@interface Core : NSObject
@property (nonatomic, weak) id<JSLDelegate> delegate;
- (Env *)env;
@end

NS_ASSUME_NONNULL_END
