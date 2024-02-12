//
//  DLPrinter.h
//  DreamLisp
//
//  Created by Jaseem V V on 05/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLTypes.h"
#import "DLConst.h"
#import <objc/objc-runtime.h>

NS_ASSUME_NONNULL_BEGIN

@interface DLPrinter : NSObject
- (nullable NSString *)printStringFor:(id<DLDataProtocol>)data readably:(BOOL)readably;
@end

NS_ASSUME_NONNULL_END
