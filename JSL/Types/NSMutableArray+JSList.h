//
//  NSMutableArray+JSList.h
//  JSL
//
//  Created by jsloop on 13/04/19.
//  Copyright © 2019 jsloop. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface NSMutableArray (JSList)
- (id)first;
- (id)second;
- (NSMutableArray *)rest;
- (id)last;
- (id)dropLast;
- (id)nth:(NSInteger)n;
- (BOOL)isEmpty;
- (NSMutableArray *)reverse;
@end

NS_ASSUME_NONNULL_END
