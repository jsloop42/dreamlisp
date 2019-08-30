//
//  DLDelegate.h
//  DreamLisp
//
//  Created by jsloop on 15/06/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLDataProtocol.h"
#import "IOService.h"

NS_ASSUME_NONNULL_BEGIN

/** A DL delegate which can be used to invoke the methods defined in @c DreamLisp.h. */
@protocol DLDelegate <NSObject>
- (id<DLDataProtocol>)eval:(id<DLDataProtocol>)ast;
- (IOService *)ioService;
@end

NS_ASSUME_NONNULL_END
