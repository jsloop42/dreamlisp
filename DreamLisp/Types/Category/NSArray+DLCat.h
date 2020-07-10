//
//  NSArray+DLDataProtocol.h
//  DreamLisp
//
//  Created by Jaseem V V on 13/04/19.
//  Copyright © 2019 Jaseem V V. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DLLogger.h"

NS_ASSUME_NONNULL_BEGIN

@interface NSArray (DLCat)
- (id)first;
- (id)second;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (BOOL)isEmpty;
@end

NS_ASSUME_NONNULL_END
