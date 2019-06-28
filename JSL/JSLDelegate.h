//
//  JSLDelegate.h
//  JSL
//
//  Created by jsloop on 15/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSDataProtocol.h"
#import "IOService.h"

NS_ASSUME_NONNULL_BEGIN

/** A JSL delegate which can be used to invoke the methods defined in @c JSL. */
@protocol JSLDelegate <NSObject>
- (id<JSDataProtocol>)eval:(id<JSDataProtocol>)ast;
- (IOService *)ioService;
@end

NS_ASSUME_NONNULL_END
