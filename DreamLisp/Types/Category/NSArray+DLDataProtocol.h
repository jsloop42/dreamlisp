//
//  NSArray+DLDataProtocol.h
//  DreamLisp
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 DreamLisp. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSArray (DLDataProtocol)
- (id)first;
- (id)second;
- (NSMutableArray *)map:(id (^)(id arg))block;
- (BOOL)isEmpty;
@end

NS_ASSUME_NONNULL_END
