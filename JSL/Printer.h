//
//  Printer.h
//  JSL
//
//  Created by jsloop on 05/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Types.h"
#import "Const.h"
#import <objc/objc-runtime.h>

NS_ASSUME_NONNULL_BEGIN

@interface Printer : NSObject
- (nullable NSString *)printStringFor:(id<JSDataProtocol>)data readably:(BOOL)readably;
@end

NS_ASSUME_NONNULL_END
