//
//  Core.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "Reader.h"
#import "Printer.h"
#import "Logger.h"
#import <objc/message.h>

NS_ASSUME_NONNULL_BEGIN

@interface Core : NSObject
- (NSMutableDictionary *)namespace;
@end

NS_ASSUME_NONNULL_END
